#lang racket

(require peg/peg)
(struct soma (a b))
(struct sub (a b))
(struct prod (a b))
(struct div (a b))
(struct parenteses (a))


(define-peg number (name value (+ (range #\0 #\9))) (string->number value))
(define-peg variable (and 
                          (or (range #\a #\z) (range #\A #\Z))
                          (* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))


(define (norm k)
                  (match k
                           [(div a (div b c)) (div (div a b) (norm c))]
                           [a a]))
 ;produto e divisão são operações definidas sobre campos
(define-peg fieldOp (and (name t1 (or number variable)) (? (and (name op (or #\* #\/)) (name t2 fieldOp))))
                  (if t2 (if (equal? op "*") (prod t1 t2) (norm (div t1 t2))) t1 ))
 
;soma e subtração são operações definidas sobre grupos
(define-peg groupOp (and (name t1 fieldOp) (? (and (name op (or #\+ #\-)) (name t2 groupOp))))
                  (if t2 (if (equal? op "+") (soma t1 t2) (sub t1 t2)) t1 ))


(define-peg aritExp groupOp)
