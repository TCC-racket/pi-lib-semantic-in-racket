#lang racket
(require peg/peg)
(require racket/lazy-require)

(require "AritExp.rkt")
(require (rename-in "BoolExp.rkt" [or orB]))
(provide peg-rule:exp )

(lazy-require ["Comando.rkt" (callF? comandoConv)] )

(struct Exp (U AritExp BoolExp))
(define-peg exp (or aritExp boolExp))

(define (expConv exp) (cond
                        [(aritExp? exp) (aritConv exp)]
                        [(boolExp? exp)  (boolConv exp)]
                        [(callF? exp) (comandoConv exp)]))
(provide expConv)
