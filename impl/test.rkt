#lang rosette

(require
    regraph
)

(define pats `(
    [(bvmul a (bvmul b c)) (bvmul (bvmul a b) c)]
    [(bvmul a b) (bvmul b a)]
    [(bvmul a 0) 0]
    [(bvmul a a) (bvsqr a)]
    [((bvadd (bvadd (bvsqr a) (bvmul (bvmul a b) 2)) (bvsqr b))) (bvsqr (bvadd a b))]

    [(bvshl a 1) (bvmul a 2)]
    [(bvshl a 2) (bvmul a 4)]

    [(bvadd a (bvmul a i)) (bvmul a (bvadd i 1))]
    [(bvadd a a) (bvshl a 1)]
    [(bvadd a 0) a]
))

(define ipats (for/list ([p pats]) (first p)))
(define opats (for/list ([p pats]) (second p)))

(define (constant-fold op . args)
    (match args
        [(list a b) #:when (and (integer? a) (integer? b))
            (match op
                ['bvadd (+ a b)]
                ['bvsub (- a b)]
                ['bvmul (* a b)]
                ['bvsdiv (/ a b)]
                [_ #f]
            )
        ]
        [_ #f]
    )
)

(define-symbolic x (bitvector 32))

(define expr0 `(bvshl ,x 2))

(define sym-quote `(bvadd ,x ,expr0))

(define rg (make-regraph (list sym-quote) #:limit 10000))
(define (loop i)
    (when (< i 50)
        (begin
            ((rule-phase ipats opats) rg)
            ((precompute-phase constant-fold) rg)
            (extractor-phase rg)
            (loop (add1 i))
        )
    )
)
(loop 0)
(define ret (first (regraph-extract rg)))
(pretty-display ret)