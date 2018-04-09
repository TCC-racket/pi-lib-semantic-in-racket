#lang racket

(require peg/peg)
(provide peg-rule:reserv)

(define-peg var "var")
(define-peg const "const")
(define-peg bar "|")
(define-peg init "init")
(define-peg proc "proc")
(define-peg module "Module")
(define-peg end "end")
(define-peg virg ",")
 
(define-peg reserv (or module end proc var init bar const ))
