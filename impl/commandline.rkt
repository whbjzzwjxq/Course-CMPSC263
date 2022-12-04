#lang racket/base

(require global)

(provide (all-defined-out))

(define-global:string *input*
  "outputs/example-opt-rewrite2.json"
  "Input LLVM IR extracted JSON file"
)

(define-global:boolean *debug*
  #f
  "Debug mode"
)

(define-global:boolean *print-program*
  #f
  "Print the whole program"
)

(define-global:boolean *optimize*
  #f
  "Optimize the whole program"
)

(void (globals->command-line #:program "LLVM interpreter and optimizer based on rosette"))
