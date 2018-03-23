#lang typed/racket
(module exp racket

  (require peg/peg)


  (require AritExp)
  (require BoolExp)

  (struct Exp (U AritExp BoolExp))
  (define-peg exp (or aritExp boolExp))

  (: parseExp (-> String Exp))
  (define (parseExp s)
  (peg exp s)))
