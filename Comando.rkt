#lang racket

(require peg/peg)
(require "atribuicao.rkt")
(require "BoolExp.rkt")
(require "AritExp.rkt")
(require "Reservadas.rkt")
(require "espacos.rkt")


(provide peg-rule:seq)
(provide peg-rule:choiceOp)
(provide peg-rule:comando)
(provide peg-rule:cmdUnit)

(struct comando (U seq init atrib print exit) #:transparent)
(struct seq (comando1 comando2) #:transparent)
(struct prnt(a) #:transparent)
(struct choice (comando1 comando2) #:transparent)
(struct exit(a))
(struct nop () #:transparent)

(define-peg separador(or virg pointvirg newLines))

(define-peg seq(and (name t1 cmdUnit) (?(name sep separador) (name t2 seq))) (if t2 (seq t1 t2) t1))

(define-peg print(and print"(" spaces (name t1 (or aritExp variable string boolExp)) spaces ")") (prnt t1))

(define-peg exit(and exit"(" spaces (name t1 (or aritExp boolExp)) spaces ")") (exit t1))

(define-peg cmdUnit (or inicializacao atributions condicional loop print exit))

(define-peg comando (or seq choiceOp cmdUnit))

(define-peg choiceOp (and (name t1 cmdUnit) (? (and bar (name t2 choiceOp)))) (choice t1 t2))


;condicionais

(provide peg-rule:condicional)

(struct if (condicao corpo) #:transparent)
(struct ifElse (condicao corpoIf corpoElse) #:transparent)
(struct condicional (U if ifElse) #:transparent)

(define-peg ifElse (and
                  "if" spaces (name condicao boolExp) spaces "{" wordSeparator (name corpoIf comando) wordSeparator "}" wordSeparator
                   "else" spaces "{" wordSeparator (name corpoElse comando) wordSeparator "}" ) (ifElse condicao corpoIf corpoElse))

(define-peg if (and
                  "if" spaces (name condicao boolExp) spaces "{" wordSeparator (name corpo comando) wordSeparator "}") (if condicao corpo))

(define-peg condicional (or ifElse if))

;loop

(provide peg-rule:loop)

(struct whileDo (condicao corpo)  #:transparent)

(define-peg loop (and
"while" spaces (name condicao boolExp) wordSeparator "do" spaces "{" wordSeparator (name corpo comando) wordSeparator "}") (whileDo 
condicao corpo))


