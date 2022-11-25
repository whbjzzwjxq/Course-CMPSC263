#lang rosette


(require
  dyoo-while-loop
  json
  racket/struct
  racket/string
  "datatype.rkt"
  "hash-vector.rkt"
  "utils.rkt"
)

(provide (all-defined-out))

(define (opd-int? operand)
  (&&
    (llvm-integer? (operand-type operand))
    (string-prefix? (datatype-name (operand-type operand)) "i")
  )
)

(define (opd-symbolic? operand)
  (&&
    (llvm-integer? (operand-type operand))
    (string-prefix? (datatype-name (operand-type operand)) "s")
  )
)

(define (opd-num? operand)
  (||
    (opd-int? operand)
  )
)

(define (opd-var? operand)
  (&&
    (non-empty-string? (operand-var-name operand))
    (opd-num? operand)
  )
)

(define (opd-const? operand)
  (&&
    (not (non-empty-string? (operand-var-name operand)))
    (opd-num? operand)
  )
)

(define (opd-label? operand)
  (equal? (operand-type operand) llvm-label)
)

(define (opd-pointer? operand)
  (llvm-pointer? (operand-type operand))
)

(define (str2datatype type-str)
  (match type-str
    ["I1" i1]
    ["I8" i8]
    ["I16" i16]
    ["I32" i32]
    ["I64" i64]
    ["I128" i128]
    ["Label" llvm-label]
    ["Struct" llvm-struct]
    ["Function" llvm-function]
    ["Void" llvm-void]

    [(regexp #rx"Pointer-.*") (let*
      (
        [new-type-str (string-trim type-str "Pointer-" #:right? #f)]
      )
      (pointer-type-init (str2datatype new-type-str))
    )]

    [(regexp #rx"Array-.*-.*") (let*
      (
        [new-type-str (string-trim type-str "Array-" #:right? #f)]
        [res (string-split new-type-str "-")]
        [num-str (first res)]
        [new-type-str1 (string-trim new-type-str (format "~a-" num-str) #:right? #f)]
      )
      (array-type-init (string->number num-str) (str2datatype new-type-str1))
    )]
    [else (error "Unknown type: " type-str)]
  )
)

(define (resolve-opd opd-jsexpr)
  (define var-name (hash-ref opd-jsexpr `var_name))
  (define type-str (hash-ref opd-jsexpr `type))
  (define value-str (hash-ref opd-jsexpr `value))
  (define pred-block (hash-ref opd-jsexpr `pred_block null))
  (define datatype (str2datatype type-str))
  (define value (if (non-empty-string? var-name) value-str (string->number value-str)))
  (operand var-name _datatype _value pred-block)
)

(define (resolve-inst inst-jsexpr)
  (define assign (hash-ref inst-jsexpr `assign))
  (define assign-type (hash-ref inst-jsexpr `assign_type))
  (define op (hash-ref inst-jsexpr `opcode))
  (define opds (hash-ref inst-jsexpr `operands))
  (define predicate (hash-ref inst-jsexpr `predicate null))
  (define alloca (hash-ref inst-jsexpr `alloca null))
  (define args (list))
  (begin
    (for ([opd opds]) (set! args (cons (resolve-opd opd) args)))
    ; Watch-out the order of cons.
    (inst assign (str2datatype assign-type) op (list->vector (reverse args)) predicate alloca)
  )
)

(define (resolve-bb bb-jsexpr)
  (define name (hash-ref bb-jsexpr `name))
  (define inst-jsexprs (hash-ref bb-jsexpr `insts))
  (define insts (list))
  (begin
    (for ([inst-jsexpr inst-jsexprs]) (
      set! insts (cons (resolve-inst inst-jsexpr) insts)
    ))
    ; Watch-out the order of cons.
    (bb name (reverse insts))
  )
)

(define (resolve-func func-jsexpr)
  (define name (hash-ref func-jsexpr `name))
  (define bbs (hash-ref func-jsexpr `basic_blocks))
  (define arg_opds (hash-ref func-jsexpr `args))
  (define bb-table (hash-vector-init MAXBB null))
  (define args (list))
  (begin
    (for ([bb bbs]) (
      let ([cur-bb (resolve-bb bb)])
      (hash-vector-set! bb-table (bb-name cur-bb) cur-bb)
    ))
    (for ([opd arg_opds]) (
      set! args (cons (resolve-opd opd) args)
    ))
    ; The same as resolve-inst
    (func name bb-table (list->vector (reverse args)))
  )
)

(define (resolve-program js-lines)
  (define func-table (hash-vector-init MAXFUNC null))
  (begin
    (for ([func-jsexpr js-lines])
      (let
        ([func (resolve-func func-jsexpr)])
        (hash-vector-set! func-table (func-name func) func)
      )
    )
    func-table
  )
)


; list jsexpr
(define (load-json-lines file)
  (define f (open-input-file file #:mode 'text))
  (define lines '())
  (begin
    (while #t
      (define line (read-line f))
      (if (eof-object? line)
        (break)
        (set! lines (cons (string->jsexpr line) lines))
      )
    )
    (close-input-port f)
    ; Watch-out the order of cons.
    (reverse lines)
  )
)

(define (load-program input-file-path)
  (resolve-program (load-json-lines input-file-path))
)
