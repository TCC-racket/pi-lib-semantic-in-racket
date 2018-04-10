#lang racket
(require peg/peg)
(require "BoolExp.rkt")
(require "Comando.rkt")
(require "espacos.rkt")

(provide peg-rule:loop)

(struct whileDo (condicao corpo))

(define-peg loop (and
"while" spaces (name condicao boolExp) wordSeparator "do" spaces "{" newLines tabs (name corpo comando) newLines "}") (whileDo 
condicao corpo))
