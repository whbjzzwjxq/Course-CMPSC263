#lang rosette

(require
  racket/port
  racket/file
  racket/cmdline
  errortrace
  "structs.rkt"
  "interpreter.rkt"
  "ir-loader.rkt"
  "utils.rkt"
)

(define filename
  (command-line
    #:program "llvm-optimizer"
    #:once-each
    [("-d" "--debug") "Debug mode" (debug-mode #t)]
    [("--print-program") "Print the whole program" (print-program #t)]
    [("-o" "--optimize") "Optimize the whole program" (optimize #t)]
    #:args (input)
    input
  )
)

(define program (load-program input))

(when (print-program)
  (pretty-display program)
)

(define result (interpret program init-state))
(debug-display result)
