#lang racket

(require peg/peg)

(require "BoolExp.rkt")
(require "Comando.rkt")
(require "espacos.rkt")
(provide peg-rule:condicional)

(struct if (condicao corpo) #:transparent)
(struct ifElse (condicao corpoIf corpoElse) #:transparent)
(struct condicional (U if ifElse) #:transparent)

(define-peg ifElse (and
                  "if " (name condicao boolExp) " {\n\t" (name corpoIf seq) (* #\n) "}" (* #\n)
                   "else {" (* (or #\n #\t)) (name corpoElse seq) "}" ) (ifElse condicao corpoIf corpoElse))

(define-peg if (and
                  "if" spaces (name condicao boolExp) spaces "{" wordSeparator (name corpo seq) wordSeparator "}") (if condicao corpo))

(define-peg condicional (or ifElse if))
                  
