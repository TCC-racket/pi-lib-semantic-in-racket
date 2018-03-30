#lang racket

(require "BoolExp.rkt")
(require "Comando.rkt")
(provide peg-rule:condicional)

(struct if (condicao corpo))
(struct ifElse (condicao corpoIf corpoElse))
(struct condicional (U if ifElse))

(define-peg ifElse (and
                  "if " (name condicao boolExp) " {\n\t" (name corpoIf comando) (* #\n) "}" (* #\n)
                   "else {" (* (or #\n #\t)) (name corpoElse comando) "}" ) (ifElse condicao corpoIf corpoElse))

(define-peg if (and
                  "if " (name condicao boolExp) " {\n\t" (name corpo comando) (* #\n) "}") (if condicao corpo))

(define-peg condicional (or ifElse if))
                  
