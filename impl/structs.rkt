#lang rosette/safe

(require
  racket/struct
  racket/format
  "datatype.rkt"
  "utils.rkt"
)

(provide (all-defined-out))

(struct nullptr () #:transparent)

(struct undef (type) #:transparent)

(struct program (global-hash function-hash) #:transparent)

(struct function (name arguments block-hash) #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) (~a "Function " (function-name obj) " Arguments " (function-arguments obj)))
      (lambda (obj) (zhash-vals (function-block-hash obj)))
    ))
  ]
)

(struct basicblock (name instructions) #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) (~a "Basicblock " (basicblock-name obj)))
      (lambda (obj) (basicblock-instructions obj))
    ))
  ]
)

(struct value (name type) #:transparent)

(struct instruction value (opcode operands predicate alloca-size alloca-type) #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) "inst")
      (lambda (obj)
        (let*
          (
            [name (value-name obj)]
            [assign-str (if (equal? name "") "" (~a (value-name obj) " = "))]
          )
          (list (~a
            assign-str
            (instruction-opcode obj)
            " "
            (datatype-name (value-type obj))
            " "
            (instruction-operands obj)
            " "
            (instruction-predicate obj)
            " "
            (instruction-alloca-size obj)
            " "
            (instruction-alloca-type obj)
          ))
        )
      )
    ))
  ]
)

(struct operand value (constant global symbolic)
  #:transparent
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) "operand")
      (lambda (obj)
        (list (~a
          (datatype-name (value-type obj))
          " "
          (if (operand-constant obj) "" (if (operand-global obj) "@" "%"))
          (value-name obj)
        ))
      )
    ))
  ]
)

(define (constant-operand? v)
  (&&
    (operand? v)
    (operand-constant v)
  )
)

(define (global-operand? v)
  (&&
    (operand? v)
    (operand-global v)
  )
)

(define (symbolic-operand? v)
  (&&
    (operand? v)
    (operand-symbolic v)
  )
)
