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

(define BITWIDTH 32)
(define POINTERWIDTH 32)

(define MAXFUNC 64)
(define MAXBB 64)
(define MAXVAR 256)

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

(define (bvsqr a) (bvmul a a))

(define (arith-and a b)
  (bitvector->integer
    (bvand
      (bv a default-bitvector)
      (bv b default-bitvector)
    )
  )
)

(define (arith-or a b)
  (bitvector->integer
    (bvor
      (bv a default-bitvector)
      (bv b default-bitvector)
    )
  )
)

(define (arith-xor a b)
  (bitvector->integer
    (bvxor
      (bv a default-bitvector)
      (bv b default-bitvector)
    )
  )
)

(define (arith-lshr a b)
  (bitvector->integer
    (bvlshr
      (bv a default-bitvector)
      (bv b default-bitvector)
    )
  )
)

(define (arith-ashr a b)
  (bitvector->integer
    (bvashr
      (bv a default-bitvector)
      (bv b default-bitvector)
    )
  )
)

(define (arith-shl a b)
  (bitvector->integer
    (bvshl
      (bv a default-bitvector)
      (bv b default-bitvector)
    )
  )
)

(define (print-ret prefix ret)
  (cond
    [(symbolic? ret) (pretty-display (format "~a returns ~a" prefix ret))]
    [(void? ret) (pretty-display "~a returns void" prefix)]
    [(bv? ret) (pretty-display (format "~a returns ~a" prefix (bitvector->integer ret)))]
    [(union? ret) (pretty-display "~a returns ~a" prefix (union-contents ret))]
  )
)
