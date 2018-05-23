#lang racket

(require peg/peg)
(require (rename-in "BoolExp.rkt" [and andB] [or orB]))
(require "AritExp.rkt")
(require "Reservadas.rkt")
(require "espacos.rkt")
(require "idt.rkt")
(require "Exp.rkt")


(provide peg-rule:string)
(provide peg-rule:atribuicao)
(provide peg-rule:variable)
(provide peg-rule:inicializacao)
(provide peg-rule:variavel)
(provide peg-rule:constante)
(provide peg-rule:declaracao)

(struct atribution (var value) #:transparent)
(struct declaraList (var decList) #:transparent)
(struct atribSeq (atrib1 atribSeq2) #:transparent)
(struct clauses (lista) #:transparent)
(struct constante (name) #:transparent)
(struct constanteBlk (iniSeq) #:transparent)
(struct variavel (name) #:transparent)
(struct variavelBlk (iniSeq) #:transparent)
(struct init (name val) #:transparent)
(struct iniSeq (ini iniSeq) #:transparent)
(struct decSeq (declaracao decSeq) #:transparent)

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

(define-peg variavel (? (and wordSeparator var wordSeparator (name t1 variable) (? wordSeparator virg wordSeparator (name t2 declaraVAR)))) (cond [t2 (cons (variavel t1) t2)] [else (variavel t1)]))

(define-peg constante (? (and wordSeparator const wordSeparator (name t1 variable) (? wordSeparator virg wordSeparator (name t2 declaraCONST)))) (cond [t2 (cons (constante t1) t2)] [else (constante t1)]))

(define-peg inicializacao (? (and wordSeparator init wordSeparator (name t1 variable) wordSeparator "=" wordSeparator (name t2 (or boolExp aritExp))
                               (? wordSeparator virg wordSeparator (name t3 declaraINI)))) (cond [t3 (cons (init t1 t2) t3)] [else (init t1 t2)]))

;(define-peg clause (name t1 (and variavel constante inicializacao))  (clauses t1))

(define-peg declaracao (or decSeq decUnit))

(define-peg decSeq (and wordSeparator (name t1 decUnit) (? wordSeparator pointvirg wordSeparator (name t2 decSeq))) (cond [t2 (decSeq t1 t2)] [else t1]))

(define-peg decUnit (or constanteBlk variavelBlk))

(define-peg constanteBlk (and wordSeparator const wordSeparator (name t1 iniSeq)) (constanteBlk t1))

(define-peg variavelBlk (and wordSeparator var wordSeparator (name t1 iniSeq)) (variavelBlk t1))

(define-peg inicializacaoBlk (and wordSeparator (name t1 variable) wordSeparator "=" wordSeparator (name t2 (or boolExp aritExp))) (init t1 t2))

(define-peg iniSeq (and wordSeparator (name t1 inicializacaoBlk) (? wordSeparator virg wordSeparator (name t2 iniSeq))) (cond [t2 (iniSeq t1 t2)] [else t1]))

(struct assign (idt exp) #:transparent)

(provide assign)


(struct ref (a b) #:transparent)
(struct cns (a b) #:transparent)
(struct dec (a b) #:transparent)

(provide dec ref cns)
(define (variavelTrans a)
	(match a
		[(iniSeq a b) (dec (variavelTrans a) (variavelTrans b))]
		[(init a b) (ref (idt a) (expConv b))]))

(define (constanteTrans a)
	(match a
		[(iniSeq a b) (dec (constanteTrans a) (constanteTrans b))]
		[(init a b) (cns (idt a) (expConv b))]))


(define (atribConv exp)

(match exp
	[(atribution var value) (assign (idt var)  (cond [(boolExp? value) (boolConv value)] [else (aritConv value)] ) )]
	[(decSeq a b) (dec (atribConv a) (atribConv b))]
	[(variavelBlk a) (variavelTrans a)]
	[(constanteBlk a) (constanteTrans a)]))

(provide atribConv)
