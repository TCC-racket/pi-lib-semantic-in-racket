#lang racket

(require peg/peg)
(require "idt.rkt");master hack
(require "espacos.rkt")
(require "AritExp.rkt")
(provide peg-rule:boolExp boolExp?)

(define (string->boolean k) (if (equal? k "true") true false))

(struct parenteses (a)  #:transparent)
(struct andOp (a b)  #:transparent) ;and
(struct orOp (a b)  #:transparent)  ;or
(struct neg (a)  #:transparent)     ;not
(struct eq (a b)  #:transparent)    ;==
(struct le (a b)  #:transparent)    ;<=
(struct be (a b)  #:transparent)    ;>=
(struct less (a b)  #:transparent)  ;<
(struct more (a b)  #:transparent)  ;>
(define (boolExp? exp) (if (more? exp) #t (if (less? exp) #t (if (be? exp) #t (if (le? exp) #t (if (eq? exp) #t (if (neg? exp) #t (if (orOp? exp) #t (if (andOp? exp) #t (if (parenteses? exp) #t #f))))))))))
(provide boolExp?)

;(struct boolExp (U andOp orOp neg eq le be less parenteses) #:transparent)

(define-peg boolean (name str (or "true" "false")) (string->boolean str))
                 
(define-peg equal (and spaces
                      (name eq1 (or boolean)) ;aritExp))
                      spaces
                      "=="
                      spaces
                      (name eq2 (or boolean)) spaces) ;aritExp)))
                  (eq eq1 eq2))

(define-peg ncOp (and (name nc1 aritExp) (? (and spaces (name op (or "==" ">=" "<=" ">" "<")) spaces (name nc2 aritExp))))
      (if nc2 (if (equal? op "==") (eq nc1 nc2) 
        (if (equal? op ">=") (be nc1 nc2) 
          (if (equal? op "<=") (le nc1 nc2) (if (equal? op ">") (more nc1 nc2) (less nc1 nc2))))) nc1))
 
(define-peg relacional (or equal ncOp))

(define-peg parenteses (and spaces "(" spaces (name value boolExp) spaces ")" spaces) (parenteses value))

(define-peg negation (and "~"(name n boolExp)) (neg n))

(define-peg terceiroNivel (or negation parenteses relacional boolean))

(define-peg conjuncao (and (name con1 terceiroNivel) (? (and spaces "/\\" spaces (name con2 conjuncao))))   (if con2 (andOp con1 con2) con1))

(define-peg disjuncao (and (name dis1 conjuncao) (? (and spaces "\\/" spaces (name dis2 disjuncao))))   (if dis2 (orOp dis1 dis2) dis1))

(define-peg boolExp disjuncao)

;boolExp -> ~boolExp | equal | boolean
;equal -> boolean==boolExp
;
;

(struct and (a b) #:transparent)
(struct or (a b) #:transparent)
(struct ge (a b) #:transparent)
(struct gt (a b) #:transparent)
(struct lt (a b) #:transparent)

(provide and or ge gt lt neg eq le or? or-a or-b and? and-a and-b)


(define (boolConv exp)
  (match exp [(andOp a b) (and (boolConv a) (boolConv b))]
             [(orOp a b) (or (boolConv a) (boolConv b))]
             [(be a b) (ge (aritConv a) (aritConv b))]
             [(more a b) (gt (aritConv a) (aritConv b))]
             [(less a b) (lt (aritConv a) (aritConv b))]
             [(neg a) (neg (boolConv a))]
             [(eq a b) (eq (aritConv a) (aritConv b))] ;quando expConv funcionar, mude.
             [(le a b) (le (aritConv a) (aritConv b))]
             [(parenteses a) (boolConv a)]
             [a a]))


(provide boolConv)

















