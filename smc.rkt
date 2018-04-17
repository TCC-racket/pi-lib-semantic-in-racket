#lang racket
(struct smc (val amb control) #:transparent)
;(require "Stack.rkt")
;(require "Contexto.rkt")
(require "AritExp.rkt")
(require "Comando.rkt")
(require "BoolExp.rkt")
(provide executeSMC smc)
(define (executeSMC bplc)
  (car (smc-val (smcEval (smc '() (hash) (list bplc))))))
  
(define (smcEval smcP)
  (writeln smcP)
  (match smcP [(smc d e (list (add a b) c ...)) (smcEval (smc d e (append (list a b 'add) c)))]
              [(smc d e (list (sub a b) c ...)) (smcEval (smc d e (append (list a b 'sub) c)))]
              [(smc d e (list (mult a b) c ...)) (smcEval (smc d e (append (list a b 'mult) c)))]
              [(smc d e (list (div a b) c ...)) (smcEval (smc d e (append (list a b 'div) c)))]
              
              [(smc d e (list (? number? a) b ...)) (smcEval (smc (cons a d) e b))]
              [(smc (list (? number? a) (? number? b) c ...) d (list 'add e ...)) (smcEval (smc (cons (+ a b) c) d e))]
              [(smc (list (? number? a) (? number? b) c ...) d (list 'sub e ...)) (smcEval (smc (cons (- b a) c) d e))]
              [(smc (list (? number? a) (? number? b) c ...) d (list 'mult e ...)) (smcEval (smc (cons (* a b) c) d e))]
              [(smc (list (? number? a) (? number? b) c ...) d (list 'div e ...)) (smcEval (smc (cons (/ b a) c) d e))]
              
              [(smc d e (list (? boolean? a) b ...)) (smcEval (smc (cons a d) e b ))]
              [(smc d e (list (? or? a) c ...)) (smcEval (smc d e (append (list (or-a a)  (or-b a) 'or) c))) ]
	      [(smc d e (list (? and? a) c ...)) (smcEval (smc d e (append (list (and-a a) (and-b a) 'and) c))) ]
	      [(smc d e (list (ge a b) c ...)) (smcEval (smc d e (append (list a b 'ge) c))) ]
	      [(smc d e (list (gt a b) c ...)) (smcEval (smc d e (append (list a b 'gt) c))) ]
	      [(smc d e (list (lt a b) c ...)) (smcEval (smc d e (append (list a b 'lt) c))) ]
	      [(smc d e (list (neg a) c ...)) (smcEval (smc d e (append (list a 'neg) c))) ]
	      [(smc d e (list (eq a b) c ...)) (smcEval (smc d e (append (list a b 'eq) c))) ]
	      [(smc d e (list (le a b) c ...)) (smcEval (smc d e (append (list a b 'le) c))) ]


	      [(smc (list (? boolean? a) (? boolean? b) c ...) d (list 'or e ...))  (smcEval (smc (cons (if a #t b) c) d e)) ]
	      [(smc (list (? boolean? a) (? boolean? b) c ...) d (list 'and e ...))  (smcEval (smc (cons (if a b #f) c) d e)) ]
	      [(smc (list (? number? a) (? number? b) c ...) d (list 'ge e ...))  (smcEval (smc (cons (>= b a) c) d e)) ]
	      [(smc (list (? number? a) (? number? b) c ...) d (list 'gt e ...))  (smcEval (smc (cons (> b a) c) d e)) ]

	      [(smc (list (? number? a) (? number? b) c ...) d (list 'lt e ...))  (smcEval (smc (cons (< b a) c) d e)) ]
	      [(smc (list (? number? a) (? number? b) c ...) d (list 'eq e ...))  (smcEval (smc (cons (= a b) c) d e)) ]
	      [(smc (list (? number? a) (? number? b) c ...) d (list 'le e ...))  (smcEval (smc (cons (<= b a) c) d e)) ]
	      [(smc (list (? boolean? a) c ...) d (list 'neg e ...))  (smcEval (smc (cons (not a) c) d e)) ]
	      [(smc a d (list (? nop? b) c ...)) (smcEval (smc a d c)) ]












              [a a]))

