#lang racket
(struct smc (val amb control) #:transparent)
;(require "Stack.rkt")
;(require "Contexto.rkt")
(require "AritExp.rkt")
(require (rename-in "Comando.rkt" [if ifBPLC] [print printBPLC]))
(require "BoolExp.rkt")
(require "atribuicao.rkt")
(provide executeSMC smc)
(define (executeSMC bplc)
  (smcEval (smc '() (hash) (list bplc))))
  
(define (smcEval smcP)
  (writeln smcP)
  (sleep 1)
  (match smcP [(smc (list) a (list)) (smc (list) a (list)) ]
	      [(smc d e (list (add a b) c ...)) (smcEval (smc d e (append (list a b 'add) c)))]
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

	      [(smc a b (list (? string? c) d ...)) (smcEval (smc (cons c a) b d))    ]
	      [(smc a d (list (? nop? b) c ...)) (smcEval (smc a d c)) ]
	      [(smc a d (list (? if? b) c ...)) (smcEval (smc a d (append (list (if-cond b) 'if (if-then b) (if-else b)) c))) ]
	      [(smc (list (? number? a) b ...) c (list 'if c1 c2 d ...))  (smcEval (smc b c (append (list (if (not (equal? a 0)) c1 c2)) d))) ]
	      [(smc (list (? boolean? a) b ...) c (list 'if c1 c2 d ...))  (smcEval (smc b c (append (list (if a c1 c2)) d))) ]
	      [(smc a b (list (? print? c) d ...)) (smcEval (smc a b (append (list (print-a c) 'print) d)) )]
	      [(smc (list a b ...) c (list 'print d ...)) (begin (display a) (smcEval (smc b c d) ))  ]
	      [(smc a b (list (seq c d) e ...)) (smcEval (smc a b (append (list c d) e))) ]
	      [(smc a b (list (choice c d) e ...)) (smcEval (smc a b (append (list (if (equal? 0 (random 2)) c d )) e )))]
	      [(smc a d (list (loop b e) c ...)) (smcEval (smc a d (append (list b 'loop b e) c))) ]
	      [(smc (list (? number? a) b ...) c (list 'loop c1 c2 d ...))  (smcEval (smc b c (append (if (not (equal? a 0)) (list c2 (loop c1 c2)) '()) d ))) ]
	      [(smc (list (? boolean? a) b ...) c (list 'loop c1 c2 d ...))  (smcEval (smc b c (append (if a (list c2 (loop c1 c2)) '()) d ))) ]
	
	      [(smc a b (list (assign c d) e ...)) (smcEval (smc a b (append (list d 'assign c) e)))]
	      [(smc (list a b ...) c (list 'assign (idt d) e ...)) (smcEval (smc b (hash-set c d a) e))]
	      [(smc a b (list (idt c) d ...)) (smcEval (smc (cons (hash-ref b c) a) b d))]





              [a a]))


(executeSMC 
		(seq 
			(assign (idt "x") 0)
			(loop (le (idt "x") 5)
				(seq 
					(printBPLC "executei ")
					(seq 
						(printBPLC (idt "x"))
						(seq 
							(printBPLC " vezes\n")
							(assign (idt "x") (add (idt "x") 1))))))))
