#lang racket

(require peg/peg)

(require "espacos.rkt")
;(require "atribuicao.rkt")

(struct soma (a b))
(struct sub (a b))
(struct prod (a b))
(struct div (a b))
(struct parenteses (a))
(provide peg-rule:aritExp)

(define-peg parenteses (and spaces "(" spaces (name value aritExp) spaces ")" spaces) (parenteses value))
(define-peg number (name value (+ (range #\0 #\9))) (string->number value))
(define-peg variable (and 
                          (or (range #\a #\z) (range #\A #\Z))
                          (* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))


(define (norm k)
                  (match k
                           [(div a (div b c)) (div (div a b) (norm c))]
                           [(sub a (sub b c)) (sub (sub a b) (norm c))]
                           [a a]))
 ;produto e divisão são operações definidas sobre campos
;(define-peg fieldOp (and spaces (name t1 (or parenteses number variable)) (? (and spaces (name op (or #\* #\/)) spaces 
;                            (name t2 fieldOp))))
;                  (if t2 (if (equal? op "*") (prod t1 t2) (norm (div t1 t2))) t1 ))
 
;soma e subtração são operações definidas sobre grupos
;(define-peg groupOp (and spaces (name t1 fieldOp) (? (and spaces (name op (or #\+ #\-)) spaces (name t2 groupOp))))
;                  (if t2 (if (equal? op "+") (soma t1 t2) (norm (sub t1 t2))) t1 ))
(define-peg divisoes (and (name s1 (or parenteses number variable)) (? (and "/" (name s2 divisoes)))) (norm (div s1 s2)))
(define-peg produtos (and (name s1 divisoes) (? (and "*" (name s2 produtos)))) (prod s1 s2))
(define-peg subtracoes (and (name s1 produtos) (? (and "-" (name s2 subtracoes)))) (norm (sub s1 s2)))
(define-peg somas (and (name s1 subtracoes) (? (and "+" (name s2 somas)))) (soma s1 s2))

(define-peg aritExp somas)
;(define-peg aritExp groupOp)






