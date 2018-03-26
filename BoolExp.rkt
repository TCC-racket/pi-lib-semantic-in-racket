#lang racket

(require peg/peg)

(require "AritExp.rtk")
(provide boolExp)

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
                      "=="
                      (name eq2 (or boolean))) ;aritExp)))
                  (eq eq1 eq2))

(define-peg onlyLess (and (name less1 aritExp) (? (and " < " (name less2 onlyLess))))  (if less2 ( less less1 less2 ) less1))

(define-peg onlyMore (and (name more1 onlyLess) (? (and " > " (name more2 onlyMore))))  (if more2 ( more more1 more2 ) more1))

(define-peg lessEqual (and (name le1 onlyMore) (? (and " <= " (name le2 lessEqual))))  (if le2 ( le le1 le2 ) le1))

(define-peg moreEqual (and (name be1 lessEqual) (? (and " >= " (name be2 moreEqual))))  (if be2 ( be be1 be2 ) be1))

(define-peg relacional (or equal))

(define-peg parenteses (and "(" (name value boolExp) ")") (parenteses value))

(define-peg negation (and "~" (name n boolExp)) (neg n))

(define-peg terceiroNivel (or negation parenteses relacional boolean))

(define-peg conjuncao (and (name con1 terceiroNivel) (? (and " /\\ " (name con2 conjuncao))))   (if con2 (andOp con1 con2) con1))

(define-peg disjuncao (and (name dis1 conjuncao) (? (and " \\/ " (name dis2 disjuncao))))   (if dis2 (orOp dis1 dis2) dis1))

(define-peg boolExp disjuncao)

;boolExp -> ~boolExp | equal | boolean
;equal -> boolean==boolExp
;
;












