#lang rosette/safe

(require
    "zhash/zhash.rkt"
    "commandline.rkt"
)

(provide
    (all-defined-out)
    zvoid
    zvoid?
    zhash
    make-zhash
    zhash-keys
    zhash-vals
    zhash-val-set!
    zhash-key-exists?
    zhash-has-key?
    zhash-secure-key!
    zhash-ref
    zhash-set!
)

(define BITWIDTH 64)
(define POINTERWIDTH 64)

(define MAXFUNC 64)
(define MAXBB 64)
(define MAXVAR 256)
(define MAXPHILENGTH 8)

(define MAXI32 (sub1 2e31))
(define MINI32 (- 2e31))

(define (zip a b) (map cons a b))

(define (not-null? v) (not (null? v)))

(define (int->bv int [size BITWIDTH]) (integer->bitvector int (bitvector size)))

(define (bv->int reg) (bitvector->integer reg))

(define (reg-init) (bv 0 BITWIDTH))

(define (pointer-init) (bv 0 POINTERWIDTH))

(define default-bitvector (bitvector BITWIDTH))

(define (debug-display s) (when (*debug*) (display (format "~a \n" s))))

(define (bvne a b) (not (bveq a b)))
