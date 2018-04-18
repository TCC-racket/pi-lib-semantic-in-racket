#lang racket

(require peg/peg)
(require "Comando.rkt")
(require "atribuicao.rkt")
(require "espacos.rkt")
(require "Reservadas.rkt")

(struct prog (modName Body) #:transparent)
(struct clauseList (a b) #:transparent)
(struct proc (procName Body) #:transparent)
(struct procSeq (proc seq) #:transparent)

(define-peg programa (and "(" wordSeparator module wordSeparator (name t1 token)
                          wordSeparator (name t2 clauses) wordSeparator end wordSeparator ")") (prog t1 t2))


(define-peg processo (and proc wordSeparator (name t1 token) wordSeparator "{" wordSeparator (name t2 comando) wordSeparator "}") (proc t1 t2))

(define-peg token (and
		(* (or (range #\a #\z) (range #\A #\Z) ))))

(define-peg clauses (and (? (name t1 variavel)) wordSeparator (name t2 clausesAux1)) (if t1 (clauseList t1 t2) t2))
(define-peg clausesAux1 (and (? (name t1 constante)) wordSeparator (name t2 clausesAux2))(if t1 (clauseList t1 t2) t2))
(define-peg clausesAux2 (and (? (name t1 inicializacao)) wordSeparator (name t2 seqProcesso))(if t1 (clauseList t1 t2) t2))
(define-peg seqProcesso (and (name t1 processo) (? wordSeparator (name t2 seqProcesso))) (if t2 (procSeq t1 t2) t1))

;(define-peg clauses (and (? (name t1 variavel)) wordSeparator (? (name t2 constante))
;                         wordSeparator (? (name t3 inicializacao))
;                         wordSeparator (+ (name t4 processo))))
