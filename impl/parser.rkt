#lang racket/base

(require
  racket/generator
  racket/match
  racket/sequence
  racket/string
  racket/pretty
  (only-in rosette/base/base bitvector bitvector->natural bool->bitvector bv)
  "hash-vector.rkt"
  "structs.rkt"
)

(provide
  bytes->module
  MAXGLOBAL
  MAXFUNC
  MAXBB
  MAXVAR
)

; parser

(define MAXGLOBAL 256)
(define MAXFUNC 256)
(define MAXBB 256)
(define MAXVAR 256)

(define (unnamed? v)
  (zero? (bytes-length (LLVMGetValueName2 v)))
)

(define (set-name! v cnt)
  (when (not (equal? void (typeof-ref v)))
    (LLVMSetValueName2 v (string->bytes/utf-8 (number->string cnt)))
  )
)

(define (bytes->module bstr)
  (define m (LLVMParseIRInContext bstr))
  (define td (LLVMGetModuleDataLayout m))

  (define global-hv (hash-vector-init MAXGLOBAL))
  (define function-hv (hash-vector-init MAXFUNC))

  (begin
    (let* ([counter 0])
      (for ([gv (in-globals m)])
        ; assign names to unnamed global variables (e.g., from ubsan)
        (when (unnamed? gv)
          (set-name! gv counter)
          (set! counter (add1 counter))
        )
        (hash-vector-set! global-hv (value-name (value-ref gv td)) gv)
      )
    )
    (for
      ([f (in-functions m)] #:when (normal-function? f))
      ; assign names to local variables
      (let ([counter 0])
        (for
          ([arg (in-arguments f)] #:when (unnamed? arg))
          (set-name! arg counter)
          (set! counter (add1 counter))
        )
        (for
          ([bb (in-basic-blocks f)])
          (define bbv (LLVMBasicBlockAsValue bb))
          (when (unnamed? bbv)
            (set-name! bbv counter)
            (set! counter (add1 counter))
          )
          (for
            ([insn (in-instructions bb)] #:when (and (typeof-ref insn) (unnamed? insn)))
            (set-name! insn counter)
            (set! counter (add1 counter))
          )
        )
      )

      (define fname (value-ref f td))
      (define args
        (for/list
          ([arg (in-arguments f)])
          (value-ref arg td)
        )
      )
      (define block-hv (hash-vector-init MAXBB))
      (for
        ([bb (in-basic-blocks f)])
        (define bbname (value-ref bb td))
        (define insns
          (for/list
            ([insn (in-instructions bb)])
            (define type (typeof-ref insn))
            (define assign (if (equal? type void) void (value-ref insn td)))
            (define opcode (LLVMGetInstructionOpcode insn))
            (define operands (for/list ([v (in-operands insn)]) (value-ref v td)))
            (make-instruction td insn assign type opcode operands)
          )
        )
        (hash-vector-set! block-hv bbname (basic-block bbname #f insns))
      )
      (hash-vector-set! function-hv fname (function fname #f args block-hv))
    )
  )

  (module global-hv function-hv)
)

(define (make-instruction td insn name type opcode operands)
  (define attributes null)
  (case opcode
    [(alloca)
      (set! operands (list (type->block (LLVMGetAllocatedType insn) td)))
      (set! attributes `((align . ,(LLVMGetAlignment insn))))
    ]

    ; append type and align
    [(load)
      (set! operands (append operands (list type)))
      (set! attributes `((align . ,(LLVMGetAlignment insn))))
    ]
    [(store)
      (set! operands (append operands (list (typeof-ref (LLVMGetOperand insn 0)))))
      (set! attributes `((align . ,(LLVMGetAlignment insn))))
    ]

    [(call)
      ; Function-name is the last arg in original llvm instruction.
      (set! operands (reverse operands))
    ]

    [(bitcast)
      ; Get element-type of pointer.
      (set! type (typeof-type (LLVMGetElementType (LLVMTypeOf insn))))
    ]

    ; swap br successors because it is reverse in llvm.
    [(br)
      (match operands
        [(list condition f-succ t-succ) (set! operands (list condition t-succ f-succ))]
        [_ #f]
      )
    ]

    [(phi)
      (set! operands
        (for/list ([i (in-range (LLVMCountIncoming insn))])
          (cons
            (value-ref (LLVMGetIncomingValue insn i) td)
            (value-ref (LLVMGetIncomingBlock insn i) td)
          )
        )
      )
    ]

    [(extractvalue insertvalue) (set! operands (append operands (LLVMGetIndices insn)))]

    ; emit byte offsets for gep
    [(getelementptr)
      (let
        ([type (LLVMTypeOf (LLVMGetOperand insn 0))])
        (define indices
          (for/list ([idx (cdr operands)])
            (case (LLVMGetTypeKind type)
              [(struct) (let*
                (
                  [elem-idx (bitvector->natural idx)]
                  [offset (LLVMOffsetOfElement td type elem-idx)]
                )
                (set! type (LLVMStructGetTypeAtIndex type elem-idx))
                (struct-offset offset)
              )]
              [(pointer array vector)
                (set! type (LLVMGetElementType type))
                (array-offset idx (LLVMABISizeOfType td type))
              ]
              [(integer)
                ; Todo
                null
              ]
              [else (raise-user-error 'llvm "unknown GEP type: ~a" (LLVMPrintTypeToString type))]
            )
          )
        )
        (set! operands (cons (car operands) indices))
      )
    ]

    ; extract icmp predicate
    [(icmp)
      (set! operands (append (list (LLVMGetICmpPredicate insn)) operands))
    ]

    ; add result type to casts
    [(sext zext trunc ptrtoint) (set! operands (append operands (list type)))]

    ; (switch v #:default default [(val bb) ...])
    [(switch)
      (match operands
        [(list v default cases ...)
          (set! operands (append (list v default) (split-to-pairs cases)))
        ]
      )
    ]

  )

  (instruction name type opcode operands attributes)
)

; (define (lift v td)
;   (define kind (LLVMGetValueKind v))
;   (cond
;     ; strip bitcast
;     [(and (equal? kind 'constant-expr) (equal? (LLVMGetConstOpcode v) 'bitcast))
;      (lift (LLVMGetOperand (value-> v _LLVMConstantExprRef) 0) td)]
;     ; lift global initializer
;     [(equal? kind 'global-variable) (let ([v (value-> v _LLVMGlobalVariableRef)])
;                                       (when (LLVMGetInitializer v)
;                                         (lift (LLVMGetInitializer v) td)))]
;     ; lift integer
;     [(equal? kind 'constant-int) (LLVMConstIntGetSExtValue (value-> v _LLVMConstantIntRef))]
;     ; lift string
;     [(and (equal? kind 'constant-data-array) (LLVMIsConstantString v)) (LLVMGetAsString v)]
;     ; lift struct
;     [(equal? kind 'constant-struct) (for/list ([op (in-operands v)])
;                                       (lift op td))]
;     ; default
;     [else (value-ref v td)]))

(define builtins
  (pregexp
    (string-join
      (list
        "llvm\\.bswap\\.i(16|32|64)"
        "llvm\\.lifetime\\.(start|end)\\.p0i8"
        "llvm\\.memset\\.p0i8\\.i64"
        "llvm\\.[su](add|sub|mul)\\.with\\.overflow\\.i(16|32|64)"
        "llvm\\.trap"
        "memset"
        "memzero_explicit"
      )
      ")|("
      #:before-first "^("
      #:after-last ")$"
    )
  )
)

(define (builtin? s)
  (regexp-match builtins s))

(define (ubsan? s)
  (string-prefix? s "__ubsan_handle_"))

(define (normal-function? func)
  (let
    ([s (value->name/string func)])
    (not (or (builtin? s) (ubsan? s)))
  )
)

(define (type->block type td)
  (define kind (LLVMGetTypeKind type))
  (case kind
    [(array) (core:marray (LLVMGetArrayLength type) (type->block (LLVMGetElementType type) td))]
    [(struct) (core:mstruct (LLVMABISizeOfType td type)
                       (for/list ([i (in-range (LLVMCountStructElementTypes type))])
                         (define subtype (LLVMStructGetTypeAtIndex type i))
                         (core:mfield i
                                 (LLVMOffsetOfElement td type i)
                                 (type->block (LLVMStructGetTypeAtIndex type i) td))))]
    [(integer pointer) (core:mcell (LLVMABISizeOfType td type))]
    [else (raise-user-error 'llvm "unknown type for block: ~a" (LLVMPrintTypeToString type))]))

; utility functions

(define (split-to-pairs lst)
  (match lst
    [(list x y rest ...) (cons (cons x y) (split-to-pairs rest))]
    [_ null]))

(define (value-ref v td)
  ; convert basic block
  (when (LLVMBasicBlockRef? v)
    (set! v (LLVMBasicBlockAsValue v))
  )

  (define s (value->name/string v))
  (define named? (non-empty-string? s))
  (define kind-enum (LLVMGetValueKind v))

  (case kind-enum
    ; inline asm
    ; parse the asm string as the c api doesn't expose access to inline asm
    [(inline-asm)
      (match (value->string v)
        [(pregexp #px"asm [a-z ]*\"([^\"]*)\", \"([^\"]*)\"$"
          (list _ template constraint))
          (asm template constraint)
        ]
      )
    ]

    ; constant int
    [(constant-int)
      (let*
        (
          [bit-width (LLVMGetIntTypeWidth (LLVMTypeOf v))]
          [value-str (value->string (value-> v _LLVMConstantIntRef))]
          [type-prefix (format "i~a " bit-width)]
          [value (string-trim value-str type-prefix #:right? #f)]
        )
        ; hack but work.
        (match value
          ["true" (bool->bitvector #t)]
          ["false" (bool->bitvector #f)]
          [else (bv (string->number value) bit-width)]
        )
      )
    ]

    ; null
    [(constant-pointer-null) (nullptr)]

    ; undef
    [(undef-value) (undef (typeof-ref v))]

    ; constant expressions
    [(constant-expr)
      (let*
        (
          [v (value-> v _LLVMConstantExprRef)]
          [opcode (LLVMGetConstOpcode v)]
          [operands (for/list ([op (in-operands v)]) (value-ref op td))]
        )
        (make-instruction td v #f #f opcode operands)
      )
    ]

    ; global
    [(function)
      (when (not named?)
        (raise-user-error 'llvm "no name for function: ~a" (value->string v))
      )
      s
    ]

    [(global-variable)
      (variable
        s
        (typeof-ref v)
        #t
      )
    ]

    ; local
    [(basic-block)
      (when (not named?)
        (raise-user-error 'llvm "no name for label: ~a" (value->string v))
      )
      s
    ]

    ; instruction here means assignment of instruction.
    [(argument instruction)
      (variable
        s
        (typeof-ref v)
        #f
      )
    ]

    ; debugging
    [(metadata metadata-as-value) (nullptr)]

    [else (raise-user-error 'llvm "unknown value: ~a" (value->string v))]))

(define (typeof-ref v)
  (typeof-type (LLVMTypeOf v))
)

(define (typeof-type type)
  (define kind (LLVMGetTypeKind type))
  (case kind
    [(void) void]
    [(integer) (bitvector (LLVMGetIntTypeWidth type))]
    [(struct) (for/list ([i (in-range (LLVMCountStructElementTypes type))])
                (typeof-type (LLVMStructGetTypeAtIndex type i)))]
    [else kind]
  )
)

(define value->string LLVMPrintValueToString)

(define (value->name/string v)
  (bytes->string/utf-8 (LLVMGetValueName2 v)))

(define (in-functions m)
  (in-generator (let loop ([v (LLVMGetFirstFunction m)])
                  (when v
                    (yield v)
                    (loop (LLVMGetNextFunction v))))))

(define (in-globals m)
  (in-generator (let loop ([v (LLVMGetFirstGlobal m)])
                  (when v
                    (yield v)
                    (loop (LLVMGetNextGlobal v))))))

(define (in-arguments f)
  (in-generator (let loop ([v (LLVMGetFirstParam f)])
                  (when v
                    (yield v)
                    (loop (LLVMGetNextParam v))))))

(define (in-basic-blocks f)
  (in-generator (let loop ([v (LLVMGetFirstBasicBlock f)])
                  (when v
                    (yield v)
                    (loop (LLVMGetNextBasicBlock v))))))

(define (in-instructions bb)
  (in-generator (let loop ([v (LLVMGetFirstInstruction bb)])
                  (when v
                    (yield v)
                    (loop (LLVMGetNextInstruction v))))))

(define (in-operands u)
  (set! u (value-> u _LLVMUserRef))
  (define n (LLVMGetNumOperands u))
  (sequence-map (lambda (i) (LLVMGetOperand u i)) (in-range n)))

(define (insn-opcode i)
  (LLVMGetInstructionOpcode i))
