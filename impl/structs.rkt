#lang racket/base

(require
  racket/struct
  racket/format
  rosette/base/base
  "hash-vector.rkt"
)

(provide (all-defined-out))

; LLVM
(struct module (global-hv function-hv) #:transparent)

(struct value (name type) #:transparent)

(struct function value (arguments block-hv) #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) (~a "func " (value-name obj) " args " (function-arguments obj)))
      (lambda (obj) (hash-vector-elements (function-block-hv obj)))
    ))
  ]
)

(struct basic-block value (instructions) #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) (~a "basicblock " (value-name obj)))
      (lambda (obj) (basic-block-instructions obj))
    ))
  ]
)

(define (print-type type)
  (if (bitvector? type) (~a "i" (bitvector-size type)) type)
)

(define (print-opd opd)
  (~a (if (bv? opd) (bitvector->integer opd) opd))
)

(struct instruction value (opcode operands attributes) #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) "inst")
      (lambda (obj)
        (let*
          (
            [assign (value-name obj)]
            [assign-str (if (equal? assign void) "" (~a (value-name obj) " = "))]
          )
          (list (~a
            assign-str
            (instruction-opcode obj)
            " "
            (print-type (value-type obj))
            " "
            (map print-opd (instruction-operands obj))
            " "
            (instruction-attributes obj)
          ))
        )
      )
    ))
  ]
)

(struct array-offset (index size) #:transparent)

(struct struct-offset (value) #:transparent)

(struct asm (template constraint) #:transparent)

(struct nullptr () #:transparent)

(struct undef (type) #:transparent)

(struct variable value (global)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) "var")
      (lambda (obj)
        (let*
          ([vtype (value-type obj)])
          (list (~a
            (if (variable-global obj) "@" "%")
            (value-name obj)
            " "
            (if (bitvector? vtype) (~a "i" (bitvector-size vtype)) vtype)
          ))
        )
      )
    ))
  ]
)

(define (global-variable? v)
  (and
    (variable? v)
    (variable-global v)
  )
)
