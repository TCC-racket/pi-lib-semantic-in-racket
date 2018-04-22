#lang racket

(require peg/peg)
(require "programa.rkt")

(define in (open-input-file "C:\\Users\\Pichau\\Desktop\\8 PERÍODO\\COMPILADORES\\RACKET TEST\\fact.imp"))

(define l1(lambda()(read-string 1000 in)))

(define pgrm(lambda()(peg programa (read-string 1000 in))))

;(l1) ;o que tem dentro do arquivo

(pgrm) ;peg programa "l1"

