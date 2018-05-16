#lang racket

(require peg/peg)
(require (rename-in "BoolExp.rkt" [and andB] [or orB]))
(require "AritExp.rkt")
(require "Reservadas.rkt")
(require "espacos.rkt")
(require "idt.rkt")

(provide peg-rule:string)
(provide peg-rule:atribuicao)
(provide peg-rule:variable)
(provide peg-rule:inicializacao)
(provide peg-rule:variavel)
(provide peg-rule:constante)
(provide peg-rule:declaracao)
(provide decSeq)

(struct atribution (var value) #:transparent)
(struct declaraList (var decList) #:transparent)
(struct atribSeq (atrib1 atribSeq2) #:transparent)
(struct decSeq (lista) #:transparent)
(struct constante (name) #:transparent)
(struct variavel (name) #:transparent)
(struct init (name val) #:transparent)

(provide atribution)

(define-peg string (and "\"" (* (or space (range #\a #\z) (range #\A #\Z) (range #\0 #\9) (and "\\" (range #\a #\z)))) "\""))

(define-peg variable (and
		(or(range #\a #\z) (range #\A #\Z))
		(* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))

(define-peg atribuicao (and (name t1 variable) spaces ":=" spaces (name t2 (or boolExp aritExp string))) (atribution t1 t2))

;auxiliares vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv

(define-peg declaraINI (and wordSeparator (name t1 variable) wordSeparator "=" wordSeparator (name t2 (or boolExp aritExp))
                            (? wordSeparator virg wordSeparator (name t3 declaraINI))) (cond [t3 (cons (init t1 t2) t3)] [else (list (init t1 t2))]))

(define-peg declaraVAR (and wordSeparator (name t1 variable) (? wordSeparator virg wordSeparator (name t2 declaraVAR))) (cond [t2 (cons (variavel t1) t2)] [else (list (variavel t1))]))

(define-peg declaraCONST (and wordSeparator (name t1 variable) (? wordSeparator virg wordSeparator (name t2 declaraCONST))) (cond [t2 (cons (constante t1) t2)] [else (list (constante t1))]))

;auxiliares ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

(define-peg variavel (and wordSeparator var wordSeparator (name t1 variable) (? wordSeparator virg wordSeparator (name t2 declaraVAR))) (cond [t2 (cons (variavel t1) t2)] [else (variavel t1)]))

(define-peg constante (and wordSeparator const wordSeparator (name t1 variable) (? wordSeparator virg wordSeparator (name t2 declaraCONST))) (cond [t2 (cons (constante t1) t2)] [else (constante t1)]))

(define-peg inicializacao (and init wordSeparator (name t1 variable) wordSeparator "=" wordSeparator (name t2 (or boolExp aritExp))
                               (? wordSeparator virg wordSeparator (name t3 declaraINI))) (cond [t3 (cons (init t1 t2) t3)] [else (init t1 t2)]))

(define-peg declaracao (name t1 (and variavel constante inicializacao))  (decSeq t1))

(struct assign (idt exp) #:transparent)

(provide assign)


(define (atribConv exp)

(match exp
	[(atribution var value) (assign (idt var)  (cond [(boolExp? value) (boolConv value)] [else (aritConv value)] ) )]))

(provide atribConv)
