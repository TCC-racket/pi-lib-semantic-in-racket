#lang racket

(require peg/peg)
(require "atribuicao.rkt")
(require "Reservadas.rkt")
;(require "condicionais.rkt")
;(require "Exp.rkt")
;(require "loop.rkt")

(provide peg-rule:comando)

(define-peg comando(or reserv));atribtuions print condicional exp loop