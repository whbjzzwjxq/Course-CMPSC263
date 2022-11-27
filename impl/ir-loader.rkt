#lang racket

(require
  dyoo-while-loop
  json
  "datatype.rkt"
  "utils.rkt"
  "structs.rkt"
)

(provide (all-defined-out))

(define (str2datatype type-str)
  (match type-str
    ["I1" i1]
    ["I8" i8]
    ["I16" i16]
    ["I32" i32]
    ["I64" i64]
    ["Label" llvm-basicblock]
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
  (define name-str (hash-ref opd-jsexpr `name))
  (define type-str (hash-ref opd-jsexpr `type))
  (define value-str (hash-ref opd-jsexpr `value))
  (define global? (hash-ref opd-jsexpr `is_global #f))

  (define constant? (equal? name-str ""))
  (define datatype (str2datatype type-str))
  (define var-name (if constant? value-str name-str))
  (operand var-name datatype constant? global?)
)

(define (resolve-inst inst-jsexpr)
  (define name-str (hash-ref inst-jsexpr `name))
  (define type-str (hash-ref inst-jsexpr `type))
  (define op (hash-ref inst-jsexpr `opcode))
  (define opds (hash-ref inst-jsexpr `operands))
  (define predicate (hash-ref inst-jsexpr `predicate null))
  (define alloca-size (hash-ref inst-jsexpr `alloca_size null))
  (define alloca-type (hash-ref inst-jsexpr `alloca_type null))
  (define args (list))
  (define datatype (str2datatype type-str))
  (begin
    (for ([opd opds]) (set! args (append args (list (resolve-opd opd)))))
    (instruction name-str datatype op args predicate alloca-size alloca-type)
  )
)

(define (resolve-bb bb-jsexpr)
  (define name (hash-ref bb-jsexpr `name))
  (define inst-jsexprs (hash-ref bb-jsexpr `insts))
  (define insts (list))
  (begin
    (for ([inst-jsexpr inst-jsexprs]) (
      set! insts (append insts (list (resolve-inst inst-jsexpr)))
    ))
    (basicblock name insts)
  )
)

(define (resolve-func func-jsexpr)
  (define name (hash-ref func-jsexpr `name))
  (define bbs (hash-ref func-jsexpr `basic_blocks))
  (define arg_opds (hash-ref func-jsexpr `args))
  (define bb-zhash (make-zhash MAXBB))
  (define args (list))
  (begin
    (for ([bb bbs]) (
      let* ([cur-bb (resolve-bb bb)] [bb-name (basicblock-name cur-bb)])
      (zhash-set! bb-zhash bb-name cur-bb)
    ))
    (for ([opd arg_opds]) (set! args (append args (list (resolve-opd opd)))))
    (function name args bb-zhash)
  )
)

(define (resolve-program js-lines)
  (define global-hash (make-zhash MAXVAR))
  (define func-hash (make-zhash MAXFUNC))
  (begin
    (for ([func-jsexpr js-lines])
      (let*
        ([func (resolve-func func-jsexpr)] [func-name (function-name func)])
        (zhash-set! func-hash func-name func)
      )
    )
    (program global-hash func-hash)
  )
)

; list jsexpr
(define (load-json-lines file)
  (define f (open-input-file file #:mode 'text))
  (define lines (list))
  (begin
    (while #t
      (define line (read-line f))
      (if (eof-object? line)
        (break)
        (set! lines (append lines (list (string->jsexpr line))))
      )
    )
    (close-input-port f)
    lines
  )
)

(define (load-program input-file-path)
  (resolve-program (load-json-lines input-file-path))
)
