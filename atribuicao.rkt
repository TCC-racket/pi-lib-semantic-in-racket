#lang racket

(require peg/peg)
(require (rename-in "BoolExp.rkt" [and andB] [or orB] ))
(require "AritExp.rkt")
(require "Reservadas.rkt")
(require "espacos.rkt")

(provide peg-rule:string)
(provide peg-rule:atribuicao)
(provide peg-rule:variable)
(provide peg-rule:inicializacao)

(struct atribution (value var) #:transparent)
(struct atribSeq (atrib1 atribSeq2) #:transparent)

(define-peg string (and "\"" (* (or space (range #\a #\z) (range #\A #\Z) (range #\0 #\9))) "\""))

(define-peg variable (and
		(or(range #\a #\z) (range #\A #\Z))
		(* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))

(define-peg atribuicao (and (name t1 variable) spaces (or ":=" "=") spaces (name t2 (or boolExp aritExp string))) (atribution t2 t1))
(define-peg atribAux(and spaces (name t1 atribuicao) (? (name t2 atribAux))) (if t2 (atribSeq t1 t2) t1))
(define-peg inicializacao(and init spaces (name t1 atribuicao) (? (name t2 atribAux))) (if t2 (atribSeq t1 t2) t1))


