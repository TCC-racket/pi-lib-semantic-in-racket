#lang racket
(require peg/peg)
(require "BoolExp.rkt")
(require "Comando.rkt")
(require "espacos.rkt")

(struct whileDo (condicao corpo))

;(define-peg loop (and 
;"while" (name condicao boolExp) "do {\n\t" (name corpo comando) (* #\n) "};") (whileDo condicao corpo))

(define-peg loop (and
"while" spaces (name condicao boolExp) wordSeparator "do" spaces "{" newLines tabs (name corpo comando) newLines "}") (whileDo 
condicao corpo))
