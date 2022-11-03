#lang rosette

(require
  racket/struct
  "utils.rkt"
  "hash-vector.rkt"
  "structs.rkt"
  "parser.rkt"
)

(provide (all-defined-out))

(struct
  mstate (vars frames mregions)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) "mstate")
      (lambda (obj) (list
        (mstate-vars obj)
        (mstate-mregions obj)
      ))
    ))
  ]
)

(define (mstate-init global-hv [frames (list)] [mregions (mregions-init)])
  (mstate global-hv frames mregions)
)

(define (mstate-copy _mstate)
  (begin
    (mstate
      (hash-vector-copy (mstate-vars _mstate))
      (list)
      (mregions-init)
    )
  )
)

(struct
  frame (vars func-name allocas ret-value)
  #:mutable
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) "frame")
      (lambda (obj) (list
        (frame-vars obj)
        (frame-func-name obj)
        (frame-allocas obj)
        (frame-ret-value obj)
      ))
    ))
  ]
)

(define (frame-init func-name)
  (frame
    (hash-vector-init MAXVAR null)
    func-name
    null
    void
  )
)

(define default-bitvector (bitvector (target-pointer-bitwidth)))

(define (nullptrtoint)
  (bv 0 default-bitvector)
)

(define (count-offset offset)
  (cond
    [(struct-offset? offset) (count-struct-offset (struct-offset-value offset))]
    [(array-offset? offset) (count-array-offset (array-offset-index offset) (array-offset-size offset))]
    [else (raise-user-error "unknown offset type ~a" offset)]
  )
)

(define (interpret program init-state transaction)
  (define global-state (mstate-copy init-state))
  (define global-var-hv (mstate-vars global-state))
  (define cur-frames (mstate-frames global-state))
  (define cur-mregions (mstate-mregions global-state))
  (define cur-transaction transaction)
  (define (gvar-read var-name) (hash-vector-ref global-var-hv var-name))
  (define (gvar-write! var-name v) (hash-vector-set! global-var-hv var-name v))
  (define (add-frame _frame) (set-mstate-frames! global-state (cons _frame cur-frames)))

  (define (store-by-ptr ptr value type)
    (define mblock (pointer-block cur-mregions ptr))
    (define offset (pointer-offset ptr))
    (define size (core:bvpointer (/ (bitvector-size type) 8)))
    (define path (core:mblock-path mblock offset size))
    (core:mblock-istore! mblock value path)
  )

  (define (internal-func? func-name)
    (and
        (hash-vector-contains? program func-name)
        (> (hash-vector-length (function-block-hv (hash-vector-ref program func-name))) 0)
    )
  )

  (define (interpret-ext-func func-name operands)
    ; Implementation of Ethereum Environment Interface (EEI).
    (define (getCallValue)
      (define a (list-ref operands 0))
      (define value (transaction-value cur-transaction))
      (define bv-type (bitvector 128))
      (store-by-ptr a (integer->bitvector value bv-type) bv-type)
    )

    (define (getCodeSize)
      (bv 65536 (bitvector (target-bitwidth)))
    )

    (define (revert)
      void
    )

    (define (codeCopy)
      void
    )

    (define (llvm.wasm.memory.size.i32)
      (bv 0 (bitvector (target-bitwidth)))
    )

    (define (llvm.assume)
      void
    )

    (match func-name
      ["getCallValue" (getCallValue)]
      ["revert" (revert)]
      ["getCodeSize" (getCodeSize)]
      ["codeCopy" (codeCopy)]
      ["llvm.wasm.memory.size.i32" (llvm.wasm.memory.size.i32)]
      ["llvm.assume" (llvm.assume)]
      [else (raise-user-error "unknown function name: " func-name)]
    )
  )

  (define (interpret-func func-name arg-values)
    (debug-display (~a "func: " func-name))
    (define cur-func (hash-vector-ref program func-name))
    (define cur-frame (frame-init func-name))
    (define local-var-hv (frame-vars cur-frame))
    (define (var-read var-name) (hash-vector-ref local-var-hv var-name))
    (define (var-write! var-name v) (hash-vector-set! local-var-hv var-name v))

    (define bb-hv (function-block-hv cur-func))
    (define defined-args (function-arguments cur-func))

    (define (interpret-block bb-name last-bb-name)
      (debug-display (~a "block: " bb-name))
      (define cur-block (hash-vector-ref bb-hv bb-name))
      (define (interpret-inst inst)
        (define op (instruction-opcode inst))
        (debug-display (~a "inst: " op))
        (define operands (instruction-operands inst))
        (define attributes (instruction-attributes inst))
        (define assign (value-name inst))

        (define (compute f reg-a reg-b) (f reg-a reg-b))

        (define (opd-get opd)
          (cond
            [(bv? opd) opd]
            [(variable? opd) (
              if (global-variable? opd)
                (gvar-read (value-name opd))
                (var-read (value-name opd))
            )]
            [(string? opd) opd]
            [(instruction? opd) (interpret-inst opd)]
            [(array-offset? opd)
              (array-offset
                (opd-get (array-offset-index opd))
                (opd-get (array-offset-size opd))
              )
            ]
            [else opd]
          )
        )

        (define (rrr f)
          (define a (list-ref operands 0))
          (define b (list-ref operands 1))
          (define assign-value (compute f (opd-get a) (opd-get b)))
          assign-value
        )

        ; Control flow

        (define (ret)
          (define assign-value void)
          (when (not (empty? operands))
            (set! assign-value (opd-get (first operands)))
          )
          (set-frame-ret-value! cur-frame assign-value)
        )

        (define (call)
          (define func-name (list-ref operands 0))
          (define assign-value null)
          (define operand-values (map opd-get (rest operands)))
          (if (internal-func? func-name)
            (set! assign-value (interpret-func func-name operand-values))
            (set! assign-value (interpret-ext-func func-name operand-values))
          )
          assign-value
        )

        (define (br)
          (define (branch target-bb-name)
            (interpret-block target-bb-name (value-name cur-block))
          )
          (define a (list-ref operands 0))
          (if (equal? (length operands) 1)
            (branch a)
            (if (bitvector->bool (opd-get a))
              (branch (list-ref operands 1))
              (branch (list-ref operands 2))
            )
          )
        )

        (define (icmp)
          ; Watchout order here. Reverse is right.
          (define pred (list-ref operands 0))
          (define a (list-ref operands 1))
          (define b (list-ref operands 2))

          (define f
            (match pred
              ['eq bveq]
              ['ne bvne]
              ['ugt bvugt]
              ['uge bvuge]
              ['ult bvult]
              ['ule bvule]
              ['sgt bvsgt]
              ['sge bvsge]
              ['slt bvslt]
              ['sle bvsle]
            )
          )

          (define value-a (ptrtoint (opd-get a) #f))
          (define value-b (ptrtoint (opd-get b) #f))
          (define assign-value (bool->bitvector (compute f value-a value-b)))
          assign-value
        )

        (define (zext)
          (define a (list-ref operands 0))
          (define b (list-ref operands 1))
          (define assign-value (zero-extend (opd-get a) b))
          assign-value
        )

        (define (phi)
          (define assign-value null)
          ; see phi in make-instruction if confused.
          (for
            ([opd operands] #:when (equal? (cdr opd) last-bb-name))

            (begin
              (set! assign-value (opd-get (car opd)))
            )
          )
          assign-value
        )

        (define (unreachable)
          void
        )

        ; Memory

        (define (getelementptr)
          (define a (list-ref operands 0))
          (define b (rest operands))

          (define ptr (opd-get a))
          (define offsets (map (lambda (opd) (count-offset (opd-get opd))) b))
          (pointer (pointer-base ptr) (apply bvadd (cons (pointer-offset ptr) offsets)))
        )

        (define (load)
          ; Always load as an integer, use inttoptr to convert to ptr if necessary.
          (define a (list-ref operands 0))
          (define type (list-ref operands 1))

          (define ptr (opd-get a))
          ; Hack but work
          (define ptr? (case type [(pointer) #t] [else #f]))
          (define bvsize
            (cond
              [ptr? (/ (target-pointer-bitwidth) 8)]
              [else (/ (bitvector-size type) 8)]))

          (define mblock (pointer-block cur-mregions ptr))
          (define offset (pointer-offset ptr))
          (define size (bvpointer bvsize))
          (define path (mblock-path mblock offset size))
          (define value (mblock-iload mblock path))

          ; If the type of load is pointer, convert back using inttoptr.
          (when ptr? (set! value (inttoptr value)))
          value
        )

        (define (store)
          ; Always store as integer, use ptrtoint to convert to int if necessary.
          (define a (list-ref operands 0))
          (define b (list-ref operands 1))
          (define type (list-ref operands 2))

          (define value (opd-get a))
          (define ptr (opd-get b))

          (if (global-variable? b)
            (var-write! b value)
            (begin
              (set! value (ptrtoint value #f))
              (set! type (type-of value))
              (store-by-ptr ptr value type)
            )
          )
        )

        ; alloca allocates a stack pointer; mblock is uninitialized.
        (define (alloca)
          (define block (list-ref operands 0))
          (set-frame-allocas! cur-frame (cons block (frame-allocas cur-frame)))
          (pointer block (core:bvpointer 0))
        )

        (define (inttoptr [addr (list-ref operands 0)])
          (set! addr (sign-extend addr (bitvector (target-pointer-bitwidth))))
          (define mr
            (core:guess-mregion-from-addr cur-mregions addr (bv 0 (type-of addr)))
          )
          ; Check in-bounds. Use size of 1 for sanity. (Real size will be actually checked upon access.)
          (core:bug-on (! (core:mregion-inbounds? mr addr (bv 1 (type-of addr)))))

          (pointer (core:mregion-name mr) (bvsub addr (bv (core:mregion-start mr) (type-of addr))))
        )

        (define (ptrtoint [ptr (list-ref operands 0)] [type (list-ref operands 1)])
          (cond
            [(nullptr? ptr) (nullptrtoint)]
            [(pointer? ptr) (let* (
              [mr (core:find-mregion-by-name cur-mregions (pointer-base ptr))]
              [start (core:bvpointer (core:mregion-start mr))]
            )
              (bvadd start (pointer-offset ptr))
            )]
            [else ptr]
          )
        )

        (define (memset ptr c size)
          (define mblock (pointer-block cur-mregions ptr))
          (define offset (pointer-offset ptr))
          (mblock-memset! mblock (extract 7 0 c) offset size)
          ptr
        )

        ; Just write self in assignment.
        (define (bitcast)
          (define a (list-ref operands 0))
          (define assign-value (opd-get a))
          assign-value
        )

        (match op
          [`nop (void)]
          [`add (rrr bvadd)]
          [`sub (rrr bvsub)]

          [`mul (rrr bvmul)]
          [`sdiv (rrr bvsdiv)]
          [`udiv (rrr bvudiv)]

          [`and (rrr bvand)]
          [`or  (rrr bvor)]
          [`xor (rrr bvxor)]

          [`lshr (rrr bvlshr)]
          [`ashr (rrr bvashr)]
          [`shl (rrr bvshl)]

          [`bitcast (bitcast)]
          [`zext (zext)]

          [`ret (ret)]
          [`call (call)]
          [`br (br)]
          [`icmp (icmp)]
          [`phi (phi)]
          [`unreachable (unreachable)]

          [`alloca (alloca)]
          [`load (load)]
          [`store (store)]
          [`inttoptr (inttoptr)]
          [`getelementptr (getelementptr)]

          [else (error "Simulator: undefine instruction " op)]
        )
      )
      (for
        ([cur-inst (basic-block-instructions cur-block)])
        (define assign (value-name cur-inst))
        (define assign-value (interpret-inst cur-inst))
        (when (not (equal? assign void))
          (var-write! (value-name assign) assign-value)
        )
      )
    )

    (begin
      ; Binding arguments to local variables.
      (for
        ([arg-pair (zip arg-values defined-args)])
        (let*
          (
            [arg-value (car arg-pair)]
            [arg-opd (cdr arg-pair)]
            [var-name (value-name arg-opd)]
          )
          (var-write! var-name arg-value)
        )
      )
      (add-frame cur-frame)
      (interpret-block "entry" "")
      (frame-ret-value cur-frame)
    )
  )

  (define ret (interpret-func "main" '()))
  (if (equal? ret void)
    (debug-display "Program finished.")
    (bitvector->integer ret)
  )
)
