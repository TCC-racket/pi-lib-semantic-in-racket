#lang racket

(require peg/peg)

(require "espacos.rkt")

(struct soma (a b) #:transparent)
(struct sub (a b) #:transparent)
(struct prod (a b) #:transparent)
(struct div (a b) #:transparent)
(struct parenteses (a) #:transparent)
(provide peg-rule:aritExp)

(define (reduce f l) (reduceAux f (car l) (cdr l)))
(define (reduceAux f acc l) (if (null? l) acc (reduceAux f (f acc (car l)) (cdr l))))


(define-peg parenteses (and spaces "(" spaces (name value aritExp) spaces ")" spaces) (parenteses value))
(define-peg number (name value (+ (range #\0 #\9))) (string->number value))
(define-peg variable (and 
                          (or (range #\a #\z) (range #\A #\Z))
                          (* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))


(define (normSub l) (reduce sub l))
(define (normDiv l) (reduce div l))
 ;produto e divisão são operações definidas sobre campos
;(define-peg fieldOp (and spaces (name t1 (or parenteses number variable)) (? (and spaces (name op (or #\* #\/)) spaces 
;                            (name t2 fieldOp))))
;                  (if t2 (if (equal? op "*") (prod t1 t2) (norm (div t1 t2))) t1 ))
 
;soma e subtração são operações definidas sobre grupos
;(define-peg groupOp (and spaces (name t1 fieldOp) (? (and spaces (name op (or #\+ #\-)) spaces (name t2 groupOp))))
;                  (if t2 (if (equal? op "+") (soma t1 t2) (norm (sub t1 t2))) t1 ))
(define-peg divisoes (and (name s1 (or parenteses number variable)) (? (and spaces "/" spaces (name s2 divisoes)))) (if s2 (cons s1 s2) (cons s1 '())))
(define-peg produtos (and (name s1 divisoes) (? (and spaces "*" spaces (name s2 produtos)))) (if s2 (prod (normDiv s1) s2) (normDiv s1)))
(define-peg subtracoes (and (name s1 produtos) (? (and spaces "-" spaces (name s2 subtracoes)))) (if s2 (cons s1 s2) (cons s1 '())))
(define-peg somas (and (name s1 subtracoes) (? (and spaces "+" spaces (name s2 somas)))) (if s2 (soma (normSub s1) s2) (normSub s1)))

(define-peg aritExp somas)
;(define-peg aritExp groupOp)

(struct add (a b) #:transparent)
(struct mult (a b) #:transparent)

(define (aritConv exp)
  (match exp [(soma a b) (add (aritConv a) (aritConv b))] [(sub a b) (sub (aritConv a) (aritConv b))]
                                                           [(prod a b) (mult (aritConv a) (aritConv b))]
                                                           [(div a b) (div (aritConv a) (aritConv b))] [a a]))
