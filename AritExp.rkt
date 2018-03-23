#lang racket

(require peg/peg)
(struct soma (a b))
(struct sub (a b))
(struct prod (a b))
(struct div (a b))



(define-peg number (name value (+ (range #\0 #\9))) (string->number value))

(define-peg prod (and (name p1 number) (? (and #\* (name p2 prod))))
                  (if p2 (prod p1 p2) p1))

(define-peg div (and(name d1 number) (? (and #\/ (name d2 div))))
                  (if d2 (norm (div d1 d2)) d1)) 
(define (norm k)
                  (match k
                           [(div a (div b c)) (div (div a b) (norm c))]
                           [a a]))

(define-peg sum (and (name t1 (or prod div)) (? (and #\+ (name t2 sum))))
                  (if t2 (soma t1 t2) t1 ))


(define-peg sub (and (name t1 (or prod div)) (? (and #\- (name t2 sub))))
                  (if t2 (sub t1 t2) t1 ))
                  
(define-peg aritExp (or sum sub))
