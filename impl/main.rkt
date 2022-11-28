#lang rosette

(require
  errortrace
  "commandline.rkt"
  "interpreter.rkt"
  "ir-loader.rkt"
  "optimizer.rkt"
  "utils.rkt"
)

(define program (load-program (*input*)))

(when (*print-program*)
  (pretty-display program)
)

(define ret (interpret program))
(if (*optimize*)
  (seek-optimization program ret)
  (cond
    [(symbolic? ret) (pretty-display (format "Program returns ~a" ret))]
    [(void? ret) (pretty-display "Program returns void.")]
    [(bv? ret) (pretty-display (format "Program returns ~a" (bitvector->integer ret)))]
  )
)
