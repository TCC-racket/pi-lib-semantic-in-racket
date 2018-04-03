#lang racket

(require peg/peg)
(struct atribution(a))

(define-peg variable (and
		(or(range #\a #\z) (range#\A #\Z))
		(* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))

(define-peg atribution (and ("var") (name t1 variable)  (":=") ("'") (name t2 variable)("'"))
;Confusão com as aspas, elas deveriam ser representadas assim?
