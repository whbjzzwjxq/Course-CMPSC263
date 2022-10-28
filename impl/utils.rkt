#lang rosette/safe

(provide (all-defined-out))

(define BITWIDTH 64)
(define POINTERWIDTH 64)

(define MAXFUNC 64)
(define MAXBB 64)
(define MAXVAR 256)
(define MAXPHILENGTH 8)


(define (zip a b) (map cons a b))

(define (not-null? v) (not (null? v)))

(define (int->bv int [size BITWIDTH]) (integer->bitvector int (bitvector size)))

(define (bv->int reg) (bitvector->integer reg))

(define (reg-init) (bv 0 BITWIDTH))

(define (pointer-init) (bv 0 POINTERWIDTH))
