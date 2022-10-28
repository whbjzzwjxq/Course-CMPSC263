#lang rosette/safe

(require
  racket/struct
  "utils.rkt"
)

(provide (all-defined-out))

(define max32 (expt 2 31))
(define max-i32 (sub1 max32))
(define min-i32 (- max32))

(struct datatype (name)
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) (datatype-name obj))
      (lambda (obj) "")
    ))
  ]
)

(struct llvm-integer datatype (width))

(define i1 (llvm-integer "i1" 1))
(define i8 (llvm-integer "i8" 8))
(define i16 (llvm-integer "i16" 16))
(define i32 (llvm-integer "i32" 32))
(define i64 (llvm-integer "i64" 64))
(define i128 (llvm-integer "i128" 128))

(define u128 (llvm-integer "u128" 128))

(define s32 (llvm-integer "s32" 32))


(define llvm-label (datatype "label"))
(define llvm-struct (datatype "struct"))
(define llvm-function (datatype "function"))

(struct llvm-array datatype (size item-type)
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) (format "array [~a x ~a]" (llvm-array-size obj) (llvm-array-item-type obj)))
      (lambda (obj) "")
    ))
  ]
)

(define (array-type-init size item-type)
  (llvm-array "array" size item-type)
)

(struct llvm-pointer datatype (item-type)
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) (format "~a*" (llvm-pointer-item-type obj)))
      (lambda (obj) "")
    ))
  ]
)

(define (pointer-type-init item-type)
  (llvm-pointer "pointer" item-type)
)

(define (s32-init) (
  begin
  (define-symbolic* x (bitvector BITWIDTH))
  x
))
