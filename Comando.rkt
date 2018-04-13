#lang racket

(require peg/peg)
(require "atribuicao.rkt")
(require "BoolExp.rkt")
(require "AritExp.rkt")
(require "Reservadas.rkt")

(struct comando (U seq init atrib print exit) #:transparent)
(struct seq (comando1 comando2) #:transparent)
(struct prnt(a) #:transparent)

(define-peg seq(and (name t1 comando) (or virg bar ";") (name t2 seq)) (if t2 (seq t1 t2) t1))

(define-peg print(and "print(" (name t1 (or boolExp aritExp variable string)) ")") (prnt t1))

(define-peg comando(or inicializacao atributions print))