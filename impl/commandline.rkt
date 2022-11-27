#lang racket/base

(require global)

(provide (all-defined-out))

(define-global:string *input*
  "outputs/example-basic-functioncall.json"
  "Input LLVM IR file"
)

(define-global:boolean *debug*
  #t
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

(void (globals->command-line #:program "LLVM interpreter and optimizer based on rosette."))
