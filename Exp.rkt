#lang typed/racket
(module exp racket

  (require peg/peg)


  (require "AritExp.rkt")
  (require "BoolExp.rkt")
  (provide peg-rule:exp)


  (struct Exp (U AritExp BoolExp))
  (define-peg exp (or aritExp boolExp))
