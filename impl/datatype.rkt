#lang rosette/safe

(require
  racket/struct
  "utils.rkt"
  )

(provide (all-defined-out))

(struct datatype (name size)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (datatype-name obj))
      (lambda (obj) "")
      ))
   ]
  )

(struct llvm-integer datatype ())

(define i1 (llvm-integer "i1" 1))
(define i8 (llvm-integer "i8" 8))
(define i16 (llvm-integer "i16" 16))
(define i32 (llvm-integer "i32" 32))
(define i64 (llvm-integer "i64" 64))
(define i128 (llvm-integer "i128" 128))

(define llvm-label (datatype "label" 0))
(define llvm-struct (datatype "struct" 0))
(define llvm-function (datatype "function" 0))
(define llvm-void (datatype "void" 0))

(struct llvm-array datatype (dim element-type)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (format "array [~a x ~a]" (llvm-array-dim obj) (llvm-array-element-type obj)))
      (lambda (obj) "")
      ))
   ]
  )

(define (array-type-init dim element-type)
  (llvm-array "array" (* dim (datatype-size element-type)) dim element-type)
  )

(struct llvm-pointer datatype (element-type)
  #:methods gen:custom-write
  [(define write-proc
     (make-constructor-style-printer
      (lambda (obj) (format "~a*" (llvm-pointer-element-type obj)))
      (lambda (obj) "")
      ))
   ]
  )

(define (pointer-type-init element-type)
  (llvm-pointer "pointer" POINTERWIDTH element-type)
  )
