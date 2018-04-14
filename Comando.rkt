#lang racket

(require peg/peg)
(require "atribuicao.rkt")
(require "BoolExp.rkt")
(require "AritExp.rkt")
(require "Reservadas.rkt")
(require "espacos.rkt")

(provide peg-rule:seq)

(struct comando (U seq init atrib print exit) #:transparent)
(struct seq (comando1 comando2) #:transparent)
(struct prnt(a) #:transparent)

(define-peg separador(or virg bar pointvirg newLines))

(define-peg seq(and (name t1 comando) (?(name sep separador) (name t2 seq))) (if t2 (seq t1 t2) t1))

(define-peg print(and "print(" (name t1 (or boolExp aritExp variable string)) ")") (prnt t1))

(define-peg comando(or inicializacao atributions print))

