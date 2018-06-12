#lang racket

(require peg/peg)
(require "atribuicao.rkt")
(require (rename-in "BoolExp.rkt" [and andB] [or orB]))
(require "AritExp.rkt")
(require "Reservadas.rkt")
(require "espacos.rkt")

(provide peg-rule:seq)
(provide peg-rule:choiceOp)
(provide peg-rule:comando)
(provide peg-rule:cmdUnit)
(provide nop? if? if-cond if-then if-else nop)
(provide seq choice)

(struct block (declarations commands) #:transparent)
(struct comando (U seq init atrib print exit) #:transparent)
(struct seq (comando1 comando2) #:transparent)
(struct prnt(a) #:transparent)
(struct choice (comando1 comando2) #:transparent)
(struct exit(a))
(struct nop () #:transparent)

(define-peg separador(and wordSeparator (or virg pointvirg newLines) wordSeparator))

(define-peg seq(and (name t1 cmdUnit) (?(name sep separador) (name t2 seq))) (cond [t2 (seq t1 t2)] [else t1]))

(define-peg print(and printR"(" spaces (name t1 (or aritExp variable string boolExp)) spaces ")") (prnt t1))

(define-peg exit(and exitR"(" spaces (name t1 (or aritExp boolExp)) spaces ")") (exit t1))

(define-peg cmdUnit (or inicializacao declaracao atribuicao condicional loop print exit))

(define-peg comando (or seq choiceOp cmdUnit))

(define-peg choiceOp (and (name t1 cmdUnit) (? (and bar (name t2 choiceOp)))) (choice t1 t2))


;condicionais

(provide peg-rule:condicional)

(struct ifP (condicao corpo) #:transparent)
(struct ifElse (condicao corpoIf corpoElse) #:transparent)
(struct condicional (U ifP ifElse) #:transparent)

(define-peg ifElse (and
                  "if" spaces (name condicao boolExp) spaces "{" wordSeparator (name corpoIf comando) wordSeparator "}" wordSeparator
                   "else" spaces "{" wordSeparator (name corpoElse comando) wordSeparator "}" ) (ifElse condicao corpoIf corpoElse))

(define-peg if (and
                  "if" spaces (name condicao boolExp) spaces "{" wordSeparator (name corpo comando) wordSeparator "}") (ifP condicao corpo))

(define-peg condicional (or ifElse if))

(struct if (cond then else)#:transparent)
(struct print(a)#:transparent)
(provide print print? print-a)


(provide if if? if-cond if-then if-else)


;loop

(provide peg-rule:loop)

(struct whileDo (condicao corpo)  #:transparent)

(define-peg loop (and
"while" spaces (name condicao boolExp) wordSeparator "do" spaces "{" wordSeparator (name corpo comando) wordSeparator "}") (whileDo 
condicao corpo))

(struct loop (cond corpo) #:transparent)
(provide loop)

(provide exit? exit-a)
(provide comandoConv)

(define (comandoConv exp)
	(match exp
	[(ifP condicao corpo) (if (boolConv condicao) (comandoConv corpo) (nop))]
	[(ifElse condicao then else) (if (boolConv condicao) (comandoConv then) (comandoConv else))]
	[(whileDo condicao corpo) (loop (boolConv condicao)(comandoConv corpo))]
	[(seq a b)(seq (comandoConv a)(comandoConv b))]
	[(prnt a) (print (cond [(boolExp? a) (boolConv a)] [else (aritConv a)] ))]
	[(choice a b)(choice (comandoConv a) (comandoConv b))]
	[(exit a)(exit (if (boolExp? a) (boolConv a) (aritConv a) ))]
	[(atribution a b) (atribConv (atribution a b)) ]
	))


