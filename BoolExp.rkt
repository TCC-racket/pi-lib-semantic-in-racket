#lang racket

(require peg/peg)

(struct andOp (a b))
(struct orOp (a b))
(struct neg (a))
(struct eq (a b))
(struct le (a b))
(struct be (a b))
(struct less (a b))
(struct more (a b))

(struct boolExp (U andOp orOp neg eq le be less))


;==
;not
;>=
;<=
;>
;<
;or
;and
(define-peg boolean (or "true" "false"))
(define-peg equal (and 
                      (name eq1 (or boolean boolExp)) ; aritExp))
                      "=="
                      (name eq2 (or boolean boolExp))) ;aritExp)))
                  (eq eq1 eq2))
                  
(define-peg negation (and "~" (name n boolExp)) (neg n))
(define-peg boolExp (or negation equal boolean))

;boolExp -> ~boolExp | equal | boolean
;equal -> boolean==boolExp
;
;
;












