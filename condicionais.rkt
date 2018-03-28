#lang racket

(require "BoolExp.rkt")
(require "Comando.rkt")
(provide peg-rule:if)

(struct if (condicao corpo))
(struct ifElse (condicao corpoIf corpoElse))
(struct condicional (U if ifElse))

(define-peg ifElse (and
                  "if " (name condicao boolExp) " {\n" (name corpoIf comando) (* #\n) "}" (* #\n)
                   "else" "{" (* #\n) (name corpoElse comando) "}" ) (ifElse condicao corpoIf corpoElse))

(define-peg if (and
                  "if " (name condicao boolExp) " {\n" (name corpo comando) (* #\n) "}") (if condicao corpo))

(define-peg condicional (or ifElse if))
                  
