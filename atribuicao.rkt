#lang racket

(require peg/peg)
;(require "BoolExp.rkt")

(provide (all-from-out))

(struct atribution(value var))

(define-peg string (and "\"" (* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9))) "\""))

(define-peg variable (and
		(or(range #\a #\z) (range #\A #\Z))
		(* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))

;(define-peg atributions (and (name t1 variable) (or ":=" "=") (name t2 (or boolExp aritExp string))) (atribution t1 t2))



