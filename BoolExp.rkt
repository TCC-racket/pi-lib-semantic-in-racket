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
                      (name eq1 (or boolean)) ;aritExp))
                      "=="
                      (name eq2 (or boolean))) ;aritExp)))
                  (eq eq1 eq2))
                  
;aritExp or number? v
;(define-peg ncOp (and (name t1 (or number variable)) (? (and (name op (or "==" "<=" ">=" ">" "<" )) (name t2 aritExp))))
;                  (if t2 (if (equal? op "==") (eq t1 t2) (if (equal? op "<=") (le t1 t2) (if (equal? op ">=") 
;                  (be t1 t2) (if (equal? op ">") (more t1 t2) (less t1 t2)))) t1 )) ;ncOp - stands for number condition op
;aritExp or number? ^

(define-peg relacional (or equal))

(define-peg parenteses (and "(" (name value boolExp) ")") value)

(define-peg negation (and "~" (name n boolExp)) (neg n))

(define-peg terceiroNivel (or negation parenteses relacional boolean))

(define-peg conjuncao (and (name con1 terceiroNivel) (? (and " /\\ " (name con2 conjuncao))))   (if con2 (andOp con1 con2) con1))

(define-peg disjuncao (and (name dis1 conjuncao) (? (and " \\/ " (name dis2 disjuncao))))   (if dis2 (orOp dis1 dis2) dis1))

(define-peg boolExp disjuncao)

;boolExp -> ~boolExp | equal | boolean
;equal -> boolean==boolExp
;
;
;Essa parte foi substituida por ncOp v
;--------------------------------------------------------------------------------------------------------------------                  
;(define-peg lessEqual (and 
;                      (name le1 number)
;                      "<="
;                      (name le2 number))
;                  (le le1 le2)) ;<=
                  
;(define-peg moreEqual (and 
;                      (name be1 number)
;                      ">="
;                      (name be2 number))
;                  (be be1 be2)) ;>=

;(define-peg onlyMore (and 
;                      (name more1 number)
;                      ">"
;                      (name more2 number))
;                  (more more1 more2)) ;>

;(define-peg onlyLess (and 
;                      (name less1 number)
;                      "<"
;                      (name less2 number))
;                  (less less1 less2)) ;<
;-------------------------------------------------------------------------------------------------------------
;Essa parte foi substituida por ncOp ^











