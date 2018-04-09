#lang racket

(require peg/peg)
(provide peg-rule:reserv)

(define-peg var(and "var"))
(define-peg const(and "const"))
(define-peg bar(and "|"))
(define-peg init(and "init"))
(define-peg proc(and "proc"))
(define-peg module(and "Module"))
(define-peg end(and "end"))

(define-peg reserv (or module end proc var init bar const ))