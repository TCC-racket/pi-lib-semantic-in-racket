#lang racket

(require peg/peg)
(require "Comando.rkt")
(require "atribuicao.rkt")
(require "espacos.rkt")
(require "Reservadas.rkt")

(provide peg-rule:programa)

(struct prog (modName Body) #:transparent)
(struct variavelOp (a b) #:transparent)
;(struct constanteOp (a b) #:transparent)
(struct inicializacaoOp (a b) #:transparent)
(struct proc (procName Body) #:transparent)
(struct procSeq (proc seq) #:transparent)

(define-peg programa (and comentario "(" module spaces (name t1 token)
                          wordSeparator (name t2 variavelCL) wordSeparator end wordSeparator ")") (prog t1 t2))


(define-peg processo (and proc wordSeparator (name t1 token) wordSeparator "{" wordSeparator (name t2 comando) wordSeparator "}") (proc t1 t2))

(define-peg token (and
		(* (or (range #\a #\z) "(" ")" (range #\A #\Z) ))))

;(define-peg variavelCL (and (? (name t1 variavel)) wordSeparator (name t2 constanteCL)) (if t1 (variavelOp t1 t2) t2))
;(define-peg constanteCL (and (? (name t1 constante)) wordSeparator (name t2 inicializacaoCL))(if t1 (constanteOp t1 t2) t2))
(define-peg variavelCL (and (* (name t1 (or variavel constante))) wordSeparator (name t2 inicializacaoCL)) (if t1 (variavelOp t1 t2) t2))

(define-peg inicializacaoCL (and (? (name t1 inicializacao)) wordSeparator (name t2 seqProcesso))(if t1 (inicializacaoOp t1 t2) t2))
(define-peg seqProcesso (and (name t1 processo) (? wordSeparator (name t2 seqProcesso))) (if t2 (procSeq t1 t2) t1))

(define-peg comentario (*(and "---" spaces specialtoken newLine)))
(define-peg specialtoken (and
		(* (or (range #\a #\z) space (range #\A #\Z) ))))
(define-peg anything (and
		(* (any-char))))
