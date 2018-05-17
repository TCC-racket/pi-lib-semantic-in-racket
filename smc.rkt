#lang racket
(struct smc (env val amb control) #:transparent)
;(require "Stack.rkt")
;(require "Contexto.rkt")
(require "idt.rkt")
(require "AritExp.rkt")
(require (rename-in "Comando.rkt" [if ifBPLC] [print printBPLC]))
(require "BoolExp.rkt")
(require "atribuicao.rkt")
(require "ambiente.rkt")
(provide executeSMC smc)
(define (executeSMC bplc)
  (smcEval (smc (hash) '() (hash) (list bplc))))
  
(define (smcEval smcP)
;  (writeln smcP)
;  (sleep 1)
  (match smcP [(smc env (list) a (list)) (smc env (list) a (list)) ]
	      [(smc env d e (list (add a b) c ...)) (smcEval (smc env d e (append (list a b 'add) c)))]
              [(smc env d e (list (sub a b) c ...)) (smcEval (smc env d e (append (list a b 'sub) c)))]
              [(smc env d e (list (mult a b) c ...)) (smcEval (smc env d e (append (list a b 'mult) c)))]
              [(smc env d e (list (div a b) c ...)) (smcEval (smc env d e (append (list a b 'div) c)))]
              
              [(smc env d e (list (? number? a) b ...)) (smcEval (smc env (cons a d) e b))]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'add e ...)) (smcEval (smc env (cons (+ a b) c) d e))]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'sub e ...)) (smcEval (smc env (cons (- b a) c) d e))]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'mult e ...)) (smcEval (smc env (cons (* a b) c) d e))]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'div e ...)) (smcEval (smc env (cons (/ b a) c) d e))]
              
              [(smc env d e (list (? boolean? a) b ...)) (smcEval (smc env (cons a d) e b ))]
              [(smc env d e (list (? or? a) c ...)) (smcEval (smc env d e (append (list (or-a a)  (or-b a) 'or) c))) ]
	      [(smc env d e (list (? and? a) c ...)) (smcEval (smc env d e (append (list (and-a a) (and-b a) 'and) c))) ]
	      [(smc env d e (list (ge a b) c ...)) (smcEval (smc env d e (append (list a b 'ge) c))) ]
	      [(smc env d e (list (gt a b) c ...)) (smcEval (smc env d e (append (list a b 'gt) c))) ]
	      [(smc env d e (list (lt a b) c ...)) (smcEval (smc env d e (append (list a b 'lt) c))) ]
	      [(smc env d e (list (neg a) c ...)) (smcEval (smc env d e (append (list a 'neg) c))) ]
	      [(smc env d e (list (eq a b) c ...)) (smcEval (smc env d e (append (list a b 'eq) c))) ]
	      [(smc env d e (list (le a b) c ...)) (smcEval (smc env d e (append (list a b 'le) c))) ]


	      [(smc env (list (? boolean? a) (? boolean? b) c ...) d (list 'or e ...))  (smcEval (smc env (cons (if a #t b) c) d e)) ]
	      [(smc env (list (? boolean? a) (? boolean? b) c ...) d (list 'and e ...))  (smcEval (smc env (cons (if a b #f) c) d e)) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'ge e ...))  (smcEval (smc env (cons (>= b a) c) d e)) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'gt e ...))  (smcEval (smc env (cons (> b a) c) d e)) ]

	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'lt e ...))  (smcEval (smc env (cons (< b a) c) d e)) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'eq e ...))  (smcEval (smc env (cons (= a b) c) d e)) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'le e ...))  (smcEval (smc env (cons (<= b a) c) d e)) ]
	      [(smc env (list (? boolean? a) c ...) d (list 'neg e ...))  (smcEval (smc env (cons (not a) c) d e)) ]

	      [(smc env a b (list (? string? c) d ...)) (smcEval (smc env (cons c a) b d))    ]
	      [(smc env a d (list (? nop? b) c ...)) (smcEval (smc env a d c)) ]
	      [(smc env a d (list (? if? b) c ...)) (smcEval (smc env a d (append (list (if-cond b) 'if (if-then b) (if-else b)) c))) ]
	      [(smc env (list (? number? a) b ...) c (list 'if c1 c2 d ...))  (smcEval (smc env b c (append (list (if (not (equal? a 0)) c1 c2)) d))) ]
	      [(smc env (list (? boolean? a) b ...) c (list 'if c1 c2 d ...))  (smcEval (smc env b c (append (list (if a c1 c2)) d))) ]
	      [(smc env a b (list (? print? c) d ...)) (smcEval (smc env a b (append (list (print-a c) 'print) d)) )]
	      [(smc env (list a b ...) c (list 'print d ...)) (begin (display a) (smcEval (smc env b c d) ))  ]
	      [(smc env a b (list (seq c d) e ...)) (smcEval (smc env a b (append (list c d) e))) ]
	      [(smc env a b (list (choice c d) e ...)) (smcEval (smc env a b (append (list (if (equal? 0 (random 2)) c d )) e )))]
	      [(smc env a d (list (loop b e) c ...)) (smcEval (smc env a d (append (list b 'loop b e) c))) ]
	      [(smc env (list (? number? a) b ...) c (list 'loop c1 c2 d ...))  (smcEval (smc env b c (append (if (not (equal? a 0)) (list c2 (loop c1 c2)) '()) d ))) ]
	      [(smc env (list (? boolean? a) b ...) c (list 'loop c1 c2 d ...))  (smcEval (smc env b c (append (if a (list c2 (loop c1 c2)) '()) d ))) ]
	
	      [(smc env a b (list (assign c d) e ...)) (smcEval (smc env a b (append (list d 'assign c) e)))]
	      [(smc env (list a b ...) c (list 'assign (idt d) e ...)) (let ([newMemory (atrib env c d a)]) (smcEval (smc env b newMemory e)))]
	      [(smc env a b (list (idt c) d ...)) (let ([v (identifier env b c)]) (smcEval (smc env (cons v a) b d)))]

	      [(smc env a b (list (? exit? c) d ...)) (smcEval (smc env a b (append (list (exit-a c) 'exit) d  ))) ]
	      [(smc env (list a b ...) c (list 'exit d ...) )  (exit a)  ]
	      [(smc env a b (list (blk c d) e ...)) (smcEval (smc env (cons env a) b (append (list c d 'blk) e)  )) ]
	      [(smc env (list (? env? a) b ...) c (list 'blk d ...)) (smcEval (smc a b (clean a c) d))]
	      [(smc env a m (list (dec a b) c ...)) (smcEval (smc env a m (append (list a b) c)))]
	      [(smc env v m (list (ref a b) r ...)) (smcEval (smc env v m (append (list b 'ref a) r)))]
	      [(smc env v m (list (cns a b) r ...)) (smcEval (smc env v m (append (list b 'cns a) r)))]
	      [(smc env (list a v ...) m (list 'ref (idt i) r ...)) (let-values ([(newMem newEnv) (reference env m i)]) (smcEval (smc newEnv v newMem r)))]
	      [(smc env (list a v ...) m (list 'cns (idt i) r ...)) (let ([newEnv (constant env i)]) (smcEval (smc newEnv v m r)))]


	      [a (raise (format "Desculpe, feature não implementada. O elemento é ~a\n" a))]))

(module+ test
	(require rackunit)
	;loop execução 6 vezes
	(check-match (executeSMC 
			(seq 
				(assign (idt "x") 0)
				(loop (le (idt "x") 5)
					(seq 
						(printBPLC "executei ")
						(seq 
							(printBPLC (idt "x"))
							(seq 
								(printBPLC " vezes\n")
								(assign (idt "x") (add (idt "x") 1)))))))) (smc env '() _ '()) )
	;factorial de 5. se quiser qualquer outro numero, mude o assign do x inicial para o numero que quiser
	(check-match  (executeSMC (seq (assign (idt "x") 5) (seq (assign (idt "acc") 1) (loop (ge (idt "x") 2) (seq (assign (idt "acc") (mult (idt "acc") (idt "x"))) (assign (idt "x") (sub (idt "x") 1))))))) (smc env '() (hash-table ("x" 1) ("acc" 120)) '())  )



)









