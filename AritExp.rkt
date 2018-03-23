#lang typed/racket

(require peg/peg)

(struct prod (U Number ([x : Number] [y : prod]))
(struct div (U Number  ([x : Number] [y: div]))

(define-peg number (name value (+ (range #\0 #\9))) (string->number value))

(define-peg prod (and (name p1 number) (? (and #\* (name p2 number)))))

(define-peg div (and(name d1 number) (?(and #\/ (name d2 number))))) 


(define-peg aritExp (and (or prod div) (? (and #\+ aritExp))))
