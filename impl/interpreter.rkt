#lang rosette

(require
  "utils.rkt"
  "structs.rkt"
  "datatype.rkt"
)

(provide (all-defined-out))

(define (interpret program)
  (define global-hash (program-global-hash program))
  (define func-hash (program-function-hash program))
  (define main-func (zhash-ref func-hash "main"))

  (define ret (interpret-func func-hash main-func (list)))
  ret
)

(define (interpret-intrisinc-func func-name operands)
  (define (llvm.assume) void)

  (match func-name
    ["llvm.assume" (llvm.assume)]
    [else (raise-user-error "Unknown function name: " func-name)]
  )
)

(define (interpret-func func-hash func args)
  (debug-display (~a "func: " (function-name func)))
  (define local-var-hash (make-zhash MAXVAR))
  (define (var-write! var-name v) (zhash-set! local-var-hash var-name v))

  (define bb-hash (function-block-hash func))
  (define params (function-arguments func))
  (define entry-bb (zhash-ref bb-hash "entry"))
  (define last-bb null)

  ; Binding arguments to local variables.
  (for
    ([arg-pair (zip args params)])
    (let*
      (
        [arg-value (car arg-pair)]
        [param (cdr arg-pair)]
        [var-name (value-name param)]
      )
      (var-write! var-name arg-value)
    )
  )
  (interpret-block func-hash bb-hash local-var-hash last-bb entry-bb)
)

(define (interpret-block func-hash bb-hash local-var-hash last-bb bb)
  (debug-display (~a "block: " (basicblock-name bb)))
  (define (var-write! var-name v) (zhash-set! local-var-hash var-name v))
  (define ret-value void)
  (for
    ([inst (basicblock-instructions bb)])
    (define inst-name (value-name inst))
    (define inst-value (interpret-inst func-hash bb-hash local-var-hash last-bb bb inst))
    (when (not (equal? inst-name ""))
      (var-write! inst-name inst-value)
    )
    (set! ret-value inst-value)
  )
  ret-value
)

(define (interpret-inst func-hash bb-hash local-var-hash last-bb bb inst)
  (define op (instruction-opcode inst))
  (debug-display (~a "inst: " op))
  (define (var-read var-name) (zhash-ref local-var-hash var-name))
  (define (var-write! var-name v) (zhash-set! local-var-hash var-name v))
  (define operands (instruction-operands inst))
  (define predicate (instruction-predicate inst))
  (define alloca-size (instruction-alloca-size inst))
  (define alloca-type (instruction-alloca-type inst))

  (define (opd-get opd)
    (cond
      [(constant-operand? opd) (constant->bv opd)]
      [(operand? opd) (var-read (value-name opd))]
      [(instruction? opd) (interpret-inst opd)]
      [else opd]
    )
  )

  (define (compute f reg-a reg-b) (f reg-a reg-b))

  (define (rrr f)
    (define a (list-ref operands 0))
    (define b (list-ref operands 1))
    (define value (compute f (opd-get a) (opd-get b)))
    value
  )
  ; Control flow

  (define (ret)
    (define ret-value void)
    (when (not (empty? operands))
      (set! ret-value (opd-get (first operands)))
    )
    ret-value
  )

  (define (call)
    (define func-pointer (last operands))
    (define func-name (value-name func-pointer))
    (define assign-value null)
    (define operand-values (map opd-get (drop-right operands 1)))
    ; Hacking symboblic function name.
    (if (string-contains? func-name "symbolicI")
        (datatype->symbolic (function-ret-type (zhash-ref func-hash func-name)))
        (if (zhash-has-key? func-hash func-name)
            (interpret-func func-hash (zhash-ref func-hash func-name) operand-values)
            (interpret-intrisinc-func func-name operand-values))
    )
  )

  (define (br)
    (define (branch dest-bb-opd)
      (define dest-bb (zhash-ref bb-hash (value-name dest-bb-opd)))
      (interpret-block func-hash bb-hash local-var-hash bb dest-bb)
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
    (define pred (instruction-predicate inst))
    (define a (list-ref operands 0))
    (define b (list-ref operands 1))

    (define f
      (match pred
        ["eq" bveq]
        ["ne" bvne]
        ["ugt" bvugt]
        ["uge" bvuge]
        ["ult" bvult]
        ["ule" bvule]
        ["sgt" bvsgt]
        ["sge" bvsge]
        ["slt" bvslt]
        ["sle" bvsle]
      )
    )

    (define bool-value (compute f (opd-get a) (opd-get b)))
    (define value (bool->bitvector bool-value))
    value
  )

  ; Just write self in assignment.
  (define (bitcast)
    (define a (list-ref operands 0))
    (define assign-value (opd-get a))
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
    (for
      ([opd operands] #:when (equal? (operand-prev-block opd) (basicblock-name last-bb)))

      (begin
        (debug-display (format "Phi select: ~a" (operand-prev-block opd)))
        (set! assign-value (opd-get opd))
      )
    )
    assign-value
  )

  (define (unreachable)
    void
  )

  ; Memory

  ; (define (getelementptr)
  ;   (define a (list-ref operands 0))
  ;   (define b (rest operands))

  ;   (define ptr (opd-get a))
  ;   (define offsets (map (lambda (opd) (count-offset (opd-get opd))) b))
  ;   (pointer (pointer-base ptr) (apply bvadd (cons (pointer-offset ptr) offsets)))
  ; )

  ; (define (load)
  ;   ; Always load as an integer, use inttoptr to convert to ptr if necessary.
  ;   (define a (list-ref operands 0))
  ;   (define type (list-ref operands 1))

  ;   (define ptr (opd-get a))
  ;   ; Hack but work
  ;   (define ptr? (case type [(pointer) #t] [else #f]))
  ;   (define bvsize
  ;     (cond
  ;       [ptr? (/ (target-pointer-bitwidth) 8)]
  ;       [else (/ (bitvector-size type) 8)]))

  ;   (define mblock (pointer-block cur-mregions ptr))
  ;   (define offset (pointer-offset ptr))
  ;   (define size (bvpointer bvsize))
  ;   (define path (mblock-path mblock offset size))
  ;   (define value (mblock-iload mblock path))

  ;   ; If the type of load is pointer, convert back using inttoptr.
  ;   (when ptr? (set! value (inttoptr value)))
  ;   value
  ; )

  ; (define (store)
  ;   ; Always store as integer, use ptrtoint to convert to int if necessary.
  ;   (define a (list-ref operands 0))
  ;   (define b (list-ref operands 1))
  ;   (define type (list-ref operands 2))

  ;   (define value (opd-get a))
  ;   (define ptr (opd-get b))

  ;   (if (global-variable? b)
  ;     (var-write! b value)
  ;     (begin
  ;       (set! value (ptrtoint value #f))
  ;       (set! type (type-of value))
  ;       (store-by-ptr ptr value type)
  ;     )
  ;   )
  ; )

  ; ; alloca allocates a stack pointer; mblock is uninitialized.
  ; (define (alloca)
  ;   (define block (list-ref operands 0))
  ;   (set-frame-allocas! cur-frame (cons block (frame-allocas cur-frame)))
  ;   (pointer block (core:bvpointer 0))
  ; )

  ; (define (inttoptr [addr (list-ref operands 0)])
  ;   (set! addr (sign-extend addr (bitvector (target-pointer-bitwidth))))
  ;   (define mr
  ;     (core:guess-mregion-from-addr cur-mregions addr (bv 0 (type-of addr)))
  ;   )
  ;   ; Check in-bounds. Use size of 1 for sanity. (Real size will be actually checked upon access.)
  ;   (core:bug-on (! (core:mregion-inbounds? mr addr (bv 1 (type-of addr)))))

  ;   (pointer (core:mregion-name mr) (bvsub addr (bv (core:mregion-start mr) (type-of addr))))
  ; )

  ; (define (ptrtoint [ptr (list-ref operands 0)] [type (list-ref operands 1)])
  ;   (cond
  ;     [(nullptr? ptr) (nullptrtoint)]
  ;     [(pointer? ptr) (let* (
  ;       [mr (core:find-mregion-by-name cur-mregions (pointer-base ptr))]
  ;       [start (core:bvpointer (core:mregion-start mr))]
  ;     )
  ;       (bvadd start (pointer-offset ptr))
  ;     )]
  ;     [else ptr]
  ;   )
  ; )

  ; (define (memset ptr c size)
  ;   (define mblock (pointer-block cur-mregions ptr))
  ;   (define offset (pointer-offset ptr))
  ;   (mblock-memset! mblock (extract 7 0 c) offset size)
  ;   ptr
  ; )
  (match op
    ["nop" (void)]
    ["add" (rrr bvadd)]
    ["sub" (rrr bvsub)]

    ["mul" (rrr bvmul)]
    ["sdiv" (rrr bvsdiv)]
    ["udiv" (rrr bvudiv)]

    ["and" (rrr bvand)]
    ["or"  (rrr bvor)]
    ["xor" (rrr bvxor)]

    ["lshr" (rrr bvlshr)]
    ["ashr" (rrr bvashr)]
    ["shl" (rrr bvshl)]

    ["bitcast" (bitcast)]
    ["zext" (zext)]

    ["ret" (ret)]
    ["call" (call)]
    ["br" (br)]
    ["icmp" (icmp)]
    ["phi" (phi)]
    ["unreachable" (unreachable)]

    ; ["alloca" (alloca)]
    ; ["load" (load)]
    ; ["store" (store)]
    ; ["inttoptr" (inttoptr)]
    ; ["getelementptr" (getelementptr)]

    [else (error "Interpreter error: Undefined instruction " op)]
  )
)
