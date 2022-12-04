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
(print-ret "Interpreter" ret)
(when (*optimize*)
  (print-ret "Optimizer" (seek-optimization program ret))
)
