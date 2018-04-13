#lang racket

(require peg/peg)
(provide peg-rule:reserv)
(provide peg-rule:var)
(provide peg-rule:const)
(provide peg-rule:bar)
(provide peg-rule:init)
(provide peg-rule:proc)
(provide peg-rule:module)
(provide peg-rule:end)
(provide peg-rule:virg)

(define-peg var "var")
(define-peg const "const")
(define-peg bar "|")
(define-peg init "init")
(define-peg proc "proc")
(define-peg module "Module")
(define-peg end "end")
(define-peg virg ",")
 
(define-peg reserv (or module end proc var init bar const ))
