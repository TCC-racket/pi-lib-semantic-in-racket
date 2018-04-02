#lang racket

(require peg/peg)

(require "espacos.rkt")
(require "AritExp.rkt")
(provide peg-rule:boolExp)

(struct parenteses (a))
(struct andOp (a b)) ;and
(struct orOp (a b))  ;or
(struct neg (a))     ;not
(struct eq (a b))    ;==
(struct le (a b))    ;<=
(struct be (a b))    ;>=
(struct less (a b))  ;<
(struct more (a b))  ;>

(struct boolExp (U andOp orOp neg eq le be less parenteses))

(define-peg boolean (or "true" "false"))
                 
(define-peg equal (and 
                      (name eq1 (or boolean)) ;aritExp))
                      spaces
                      "=="
                      spaces
                      (name eq2 (or boolean))) ;aritExp)))
                  (eq eq1 eq2))

(define-peg ncOp (and (name nc1 aritExp) (? (and (name op (or " >= " " <= " " > " " < ")) (name nc2 relacional))))
      (if nc2 (if (equal? op " >= ") (be nc1 nc2) 
        (if (equal? op " <= ") (le nc1 nc2) 
          (if (equal? op " > ") (more nc1 nc2) (less nc1 nc2)))) nc1))
 
(define-peg relacional (or equal ncOp))

(define-peg parenteses (and "(" (name value boolExp) ")") (parenteses value))

(define-peg negation (and "~" (name n boolExp)) (neg n))

(define-peg terceiroNivel (or negation parenteses relacional boolean))

(define-peg conjuncao (and (name con1 terceiroNivel) (? (and " /\\ " (name con2 conjuncao))))   (if con2 (andOp con1 con2) con1))

(define-peg disjuncao (and (name dis1 conjuncao) (? (and spaces "\\/" spaces (name dis2 disjuncao))))   (if dis2 (orOp dis1 dis2) dis1))

(define-peg boolExp disjuncao)

;boolExp -> ~boolExp | equal | boolean
;equal -> boolean==boolExp
;
;
























