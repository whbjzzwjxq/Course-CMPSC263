#lang racket

(require
  racket/struct
  "utils.rkt"
)

(provide (all-defined-out))

(struct
  hash-vector (key2idx vec)
  #:methods gen:custom-write
  [(define write-proc
    (make-constructor-style-printer
      (lambda (obj) 'hash-vector)
      (lambda (obj) (list
        (hash-vector-key2idx obj)
        (hash-vector-elements obj)
      ))
    ))
  ]
)

(define (hash-vector-init vec-size init-obj)
  (hash-vector (make-hash) (make-vector vec-size init-obj))
)

(define (hash-vector-copy hash-vec)
  (hash-vector (hash-copy (hash-vector-key2idx hash-vec)) (vector-copy (hash-vector-vec hash-vec)))
)

(define (hash-vector-length hash-vec)
  (length (hash->list (hash-vector-key2idx hash-vec)))
)

(define (hash-vector-idx-ref hash-vec key)
  (hash-ref (hash-vector-key2idx hash-vec) key)
)
(define (hash-vector-obj-ref hash-vec idx)
  (vector-ref (hash-vector-vec hash-vec) idx)
)

(define (hash-vector-contains? hash-vec key)
  (>= (hash-ref (hash-vector-key2idx hash-vec) key -1) 0)
)

(define (hash-vector-ref hash-vec key)
  (begin
    (when (not (hash-vector-contains? hash-vec key))
      (hash-set! (hash-vector-key2idx hash-vec) key (hash-vector-length hash-vec))
    )
    (hash-vector-obj-ref hash-vec (hash-vector-idx-ref hash-vec key))
  )
)

(define (hash-vector-set! hash-vec key obj)
  (begin
    (when (not (hash-vector-contains? hash-vec key))
      (hash-set! (hash-vector-key2idx hash-vec) key (hash-vector-length hash-vec))
    )
    (vector-set! (hash-vector-vec hash-vec) (hash-vector-idx-ref hash-vec key) obj)
  )
)

(define (hash-vector-elements hash-vec)
  (filter not-null? (vector->list (hash-vector-vec hash-vec)))
)
