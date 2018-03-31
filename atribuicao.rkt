
#lang racket

(require peg/peg)
(struct atribution(a))

(define-peg variable (and
		(or(range #\a #\z) (range#\A #\Z))
		(* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))

(define-peg atribution (and (name t1 variable (? (and ":=" name t2 variable))))	;Provavelmente errado mas vou upar pra ver com vocês essa semana
