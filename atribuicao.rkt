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
(provide peg-rule:variavel)
(provide peg-rule:constante)
(provide peg-rule:declaracao)

(struct atribution (value var) #:transparent)
(struct declaraList (var decList) #:transparent)
(struct atribSeq (atrib1 atribSeq2) #:transparent)
(struct idt (var) #:transparent)

(define-peg string (and "\"" (* (or space (range #\a #\z) (range #\A #\Z) (range #\0 #\9))) "\""))

(define-peg variable (and
		(or(range #\a #\z) (range #\A #\Z))
		(* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))

(define-peg atribuicao (and (name t1 variable) spaces (or ":=" "=") spaces (name t2 (or boolExp aritExp string))) (atribution t2 t1))

(define-peg atribAux (and spaces (name t1 atribuicao) (? newLines virg newLines (name t2 atribAux))) (if t2 (atribSeq t1 t2) t1))
(define-peg inicializacao (and init wordSeparator (name t1 atribuicao) (? newLines virg newLines (name t2 atribAux))) (if t2 (atribSeq t1 t2) t1))

(define-peg declaraAux (and spaces (name t1 variable) (? newLines virg newLines (name t2 declaraAux))) (if t2 (declaraList t1 t2) t1))
(define-peg variavel (and var wordSeparator (name t1 variable) (? newLines virg newLines (name t2 declaraAux))) (if t2 (declaraList t1 t2) t1))
(define-peg constante (and const wordSeparator (name t1 variable) (? newLines virg newLines (name t2 declaraAux))) (if t2 (declaraList t1 t2) t1))
(define-peg declaracao (or variavel constante))



(define (atriConv exp)
(match exp
	[(atribution value var) (assign (if (boolExp? value) (boolConv value) (aritConv value)) (idt var))]))
