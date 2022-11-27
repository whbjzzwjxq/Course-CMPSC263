#lang racket

(require
  errortrace
  "commandline.rkt"
  "interpreter.rkt"
  "ir-loader.rkt"
  "utils.rkt"
)

(define program (load-program (*input*)))

(when (*print-program*)
  (pretty-display program)
)

(define result (interpret program))
(debug-display result)
