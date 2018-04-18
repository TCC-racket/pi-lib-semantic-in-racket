#lang racket
(require peg/peg)


(require "AritExp.rkt")
(require (rename-in "BoolExp.rkt" [or orB]))
(provide peg-rule:exp )


(struct Exp (U AritExp BoolExp))
(define-peg exp (or aritExp boolExp))

(define (expConv exp) (match exp)
