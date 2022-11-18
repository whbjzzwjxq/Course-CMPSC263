#lang rosette

(require
  racket/port
  racket/file
  racket/cmdline
  errortrace
  "parser.rkt"
  "structs.rkt"
  "interpreter.rkt"
  "utils.rkt"
)

(define filename
  (command-line
    #:program "llvm-optimizer"
    #:once-each
    [("+d" "++debug") "debug mode" (debug-mode #t)]
    [("++print-program") "print the whole program" (print-program #t)]
    #:args (input)
    input
  )
)

(define input-bytes (if (equal? filename "-") (port->bytes) (file->bytes filename #:mode 'binary)))
(define m (bytes->module input-bytes))

(define program (module-function-hv m))
(define global-hv (module-global-hv m))

(when (print-program)
  (pretty-display program)
)

(debug-display global-hv)

(define result (interpret program init-state))
(debug-display result)
(exit result)
