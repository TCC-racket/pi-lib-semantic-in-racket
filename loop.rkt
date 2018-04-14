#lang racket
(require peg/peg)
(require "BoolExp.rkt")
(require "Comando.rkt")
(require "espacos.rkt")

(provide peg-rule:loop)

(struct whileDo (condicao corpo)  #:transparent)

(define-peg loop (and
"while" spaces (name condicao boolExp) wordSeparator "do" spaces "{" wordSeparator (name corpo seq) wordSeparator "}") (whileDo 
condicao corpo))
