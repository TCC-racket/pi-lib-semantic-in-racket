#lang racket

(require peg/peg)

(struct andOp (a b)) ;and
(struct orOp (a b))  ;or
(struct neg (a))     ;not
(struct eq (a b))    ;==
(struct le (a b))    ;<=
(struct be (a b))    ;>=
(struct less (a b))  ;<
(struct more (a b))  ;>

(struct boolExp (U andOp orOp neg eq le be less))


(define-peg boolean (or "true" "false"))
(define-peg equal (and 
                      (name eq1 (or boolean)) ; aritExp))
                      "=="
                      (name eq2 (or boolean))) ;aritExp)))
                  (eq eq1 eq2))
(define-peg relacional (or equal))
(define-peg parenteses (and "(" (name value boolExp) ")") value)
(define-peg negation (and "~" (name n boolExp)) (neg n))
(define-peg terceiroNivel (or negation parenteses relacional boolean))
(define-peg conjuncao (and (name con1 terceiroNivel) (? (and "/\" (name con2 conjucao))))   (if con2 (andOp con1 con2) con1))
(define-peg disjuncao (and (name dis1 conjucao) (? (and "\/" (name dis2 disjuncao))))   (if dis2 (orOp dis1 dis2) dis1))
(define-peg boolExp disjuncao)

;boolExp -> ~boolExp | equal | boolean
;equal -> boolean==boolExp
;
;
;












