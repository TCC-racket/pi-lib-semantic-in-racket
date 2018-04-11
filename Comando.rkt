#lang racket

(require peg/peg)
(require "atribuicao.rkt")
(require "AritExp.rkt")

(struct comando (U seq init atrib print exit))
(struct seq (comando1 comando2))
(struct prnt(a))

(define-peg print(and "print(" (name t1 (or aritExp variable string)) ")") (prnt t1))
