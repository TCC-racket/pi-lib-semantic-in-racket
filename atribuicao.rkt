#lang racket

(require peg/peg)

(provide (all-from-out))
(require "BoolExp.rkt")
(require "AritExp.rkt")

(struct atribution(value var))

(define-peg string (and "\"" (* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9))) "\""))

(define-peg variable (and
		(or(range #\a #\z) (range #\A #\Z))
		(* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))

(define-peg atributions (and (name t1 variable) (or ":=" "=") (name t2 (or boolExp aritExp string))) (atribution t2 t1))
