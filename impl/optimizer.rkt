#lang rosette

(require
    regraph
    "utils.rkt"
)

(provide seek-optimization)

(define pats `(
    [(bvmul a (bvmul b c)) (bvmul (bvmul a b) c)]
    [(bvmul a b) (bvmul b a)]
    [(bvmul a 0) 0]
    [(bvmul a a) (bvsqr a)]
    [(bvmul a (bvadd b c)) (bvadd (bvmul a b) (bvmul a c))]
    [(bvadd (bvadd (bvsqr a) (bvmul (bvmul a b) 2)) (bvsqr b)) (bvsqr (bvadd a b))]

    [(bvshl a 1) (bvmul a 2)]
    [(bvshl a 2) (bvmul a 4)]

    [(bvadd a (bvmul a i)) (bvmul a (bvadd i 1))]
    [(bvadd a a) (bvshl a 1)]
    [(bvadd a b) (bvadd b a)]
    [(bvadd a (bvadd b c)) (bvadd (bvadd a b) c)]
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
                ['bvand (arith-and a b)]
                ['bvor (arith-or a b)]
                ['bvxor (arith-xor a b)]
                ['bvlshr (arith-lshr a b)]
                ['bvashr (arith-ashr a b)]
                ['bvshl (arith-shl a b)]
            )
        ]
        [_ #f]
    )
)

(define (symbolic->quote symbolic)
    (define ret (match symbolic
        [(expression op child1 child2) (let*
            ([a (symbolic->quote child1)] [b (symbolic->quote child2)])
            (cond
                [(equal? op bvadd) `(bvadd ,a ,b)]
                [(equal? op bvsub) `(bvsub ,a ,b)]
                [(equal? op bvmul) `(bvmul ,a ,b)]
                [(equal? op bvsdiv) `(bvsdiv ,a ,b)]
                [(equal? op bvand) `(bvand ,a ,b)]
                [(equal? op bvor) `(bvor ,a ,b)]
                [(equal? op bvxor) `(bvxor ,a ,b)]
                [(equal? op bvlshr) `(bvlshr ,a ,b)]
                [(equal? op bvashr) `(bvashr ,a ,b)]
                [(equal? op bvshl) `(bvshl ,a ,b)]
                [(equal? op bvsqr) `(bvsqr ,a ,b)]
            )
        )]
        [(expression ite cond child1 child2)
            `(ite ,cond ,(symbolic->quote child1) ,(symbolic->quote child2))]
        [(constant _ _) symbolic]
        [(bv val _) val]
    ))
    ret
)

(define (quote->symbolic symbolic)
    (match symbolic
        [(list op child1) (let*
            ([a (quote->symbolic child1)])
            (match op
                [`bvsqr (bvadd a)]
            )
        )]
        [(list op child1 child2) (let*
            ([a (quote->symbolic child1)] [b (quote->symbolic child2)])
            (match op
                [`bvadd (bvadd a b)]
                [`bvsub (bvsub a b)]
                [`bvmul (bvmul a b)]
                [`bvsdiv (bvsdiv a b)]
                [`bvand (bvand a b)]
                [`bvor (bvor a b)]
                [`bvxor (bvxor a b)]
                [`bvlshr (bvlshr a b)]
                [`bvashr (bvashr a b)]
                [`bvshl (bvshl a b)]
            )
        )]
        [(list ite cond child1 child2)
            (let* ([a (quote->symbolic child1)] [b (quote->symbolic child2)])
                (if cond a b)
            )]
        [(? number?) (bv symbolic default-bitvector)]
        [(? symbolic?) symbolic]
    )
)

(define (gen-optimize sym-quote)
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
    ret
)


(define (seek-optimization program symbolic-ret)
    (define ret (if (union? symbolic-ret)
        (for/all ([symbolic-expr symbolic-ret #:exhaustive])
            (define cond (car symbolic-expr))
            (define expr (cdr symbolic-expr))
            (quote->symbolic (gen-optimize (symbolic->quote expr)))
        )
        (quote->symbolic (gen-optimize (symbolic->quote symbolic-ret)))
    ))
    ret
)
