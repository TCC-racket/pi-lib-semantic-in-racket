#lang racket
; smc (hash? list? hash? list? (listof loc))
(struct smc (env val mem control loc) #:transparent)
(struct abs (formals block) #:transparent)

(require "idt.rkt")
(require "AritExp.rkt")
(require (rename-in "Comando.rkt" [if ifBPLC] [print printBPLC]))
(require "BoolExp.rkt")
(require "atribuicao.rkt")
(require "ambiente.rkt")
(provide executeSMC smc)
(define (executeSMC bplc)
  (smcEval (smc (hash) '() (hash) (list bplc) '())))
  
(define (smcEval smcP)
;(writeln smcP)
  ;(read)
;  (sleep 1)
  (if
  	(and (empty? (smc-val smcP)) (empty? (smc-control smcP)))
	smcP
  	(smcEval (match smcP 
	      [(smc env d e (list (add a b) c ...) locali) (smc env d e (append (list a b 'add) c) locali)]
              [(smc env d e (list (sub a b) c ...) locali) (smc env d e (append (list a b 'sub) c) locali)]
              [(smc env d e (list (mult a b) c ...) locali) (smc env d e (append (list a b 'mult) c) locali)]
              [(smc env d e (list (div a b) c ...) locali) (smc env d e (append (list a b 'div) c) locali)]
              
              [(smc env d e (list (? number? a) b ...) locali) (smc env (cons a d) e b locali)]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'add e ...) locali) (smc env (cons (+ a b) c) d e locali)]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'sub e ...) locali) (smc env (cons (- b a) c) d e locali)]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'mult e ...) locali) (smc env (cons (* a b) c) d e locali)]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'div e ...) locali) (smc env (cons (/ b a) c) d e locali)]
              
              [(smc env d e (list (? boolean? a) b ...) locali) (smc env (cons a d) e b  locali)]
              [(smc env d e (list (? or? a) c ...) locali) (smc env d e (append (list (or-a a)  (or-b a) 'or) c) locali) ]
	      [(smc env d e (list (? and? a) c ...) locali) (smc env d e (append (list (and-a a) (and-b a) 'and) c) locali) ]
	      [(smc env d e (list (ge a b) c ...) locali) (smc env d e (append (list a b 'ge) c) locali) ]
	      [(smc env d e (list (gt a b) c ...) locali) (smc env d e (append (list a b 'gt) c) locali) ]
	      [(smc env d e (list (lt a b) c ...) locali) (smc env d e (append (list a b 'lt) c) locali) ]
	      [(smc env d e (list (neg a) c ...) locali) (smc env d e (append (list a 'neg) c) locali) ]
	      [(smc env d e (list (eq a b) c ...) locali) (smc env d e (append (list a b 'eq) c) locali) ]
	      [(smc env d e (list (le a b) c ...) locali) (smc env d e (append (list a b 'le) c) locali) ]


	      [(smc env (list (? boolean? a) (? boolean? b) c ...) d (list 'or e ...) locali)  (smc env (cons (if a #t b) c) d e locali) ]
	      [(smc env (list (? boolean? a) (? boolean? b) c ...) d (list 'and e ...) locali)  (smc env (cons (if a b #f) c) d e locali) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'ge e ...) locali)  (smc env (cons (>= b a) c) d e locali) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'gt e ...) locali)  (smc env (cons (> b a) c) d e locali) ]

	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'lt e ...) locali)  (smc env (cons (< b a) c) d e locali) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'eq e ...) locali)  (smc env (cons (= a b) c) d e locali) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'le e ...) locali)  (smc env (cons (<= b a) c) d e locali) ]
	      [(smc env (list (? boolean? a) c ...) d (list 'neg e ...) locali)  (smc env (cons (not a) c) d e locali) ]

	      [(smc env a d (list (? nop? b) c ...) locali) (smc env a d c locali) ]
	      
	      [(smc env a d (list (? if? b) c ...) locali) (smc env a d (append (list (if-cond b) 'if (if-then b) (if-else b)) c) locali) ]
	      [(smc env (list (? boolean? a) b ...) c (list 'if c1 c2 d ...) locali)  (smc env b c (append (list (if a c1 c2)) d) locali) ]

;		Talvez criar mais um item semantico contendo os efeitos colaterais fosse legal, tornaria o smcEval 100% funcional, sem efeito colateral
	      [(smc env a b (list (? print? c) d ...) locali) (smc env a b (append (list (print-a c) 'print) d) locali)]
	      [(smc env (list a b ...) c (list 'print d ...) locali) (begin (display (format "~a\n" a)) (smc env b c d locali) )  ]
	      
	      [(smc env a b (list (seq c d) e ...) locali) (smc env a b (append (list c d) e) locali) ]


;		Poderia mudar isso para uso do operador 'amb', de escolha não-deterministica
;		Mas para isso precisaria da chamada de fail em algum lugar, ou seja, precisaria
;		de alguma condição sendo testada. Se for uma modal(LTL(linear temporal logic)) pode dar certo
;		Obviamente precisaria ser decidivel(não é, então assim, o que eu faço?), mas não sei como fazer com os loops infinitos
;		decidiveis
	      [(smc env a b (list (choice c d) e ...) locali)  (smc env a b (append (list (if (equal? 0 (random 2)) c d )) e ) locali)]

	      [(smc env a d (list (loop b e) c ...) locali) (smc env a d (append (list b 'loop b e) c) locali) ]

	      [(smc env (list (? boolean? a) b ...) c (list 'loop c1 c2 d ...) locali)  (smc env b c (append (if a (list c2 (loop c1 c2)) '()) d ) locali) ]
;;TODO : verify types in atrib	
	      [(smc env a b (list (assign c d) e ...) locali) (smc env a b (append (list d 'assign c) e) locali)]
	      [(smc env (list a b ...) c (list 'assign (idt d) e ...) locali) (let ([newMemory (atrib env c d a)]) (smc env b newMemory e locali))]
	      
	      [(smc env a b (list (idt c) d ...) locali) (let ([v (identifier env b c)]) (smc env (cons v a) b d locali))]

	      [(smc env a b (list (? exit? c) d ...) locali) (smc env a b (append (list (exit-a c) 'exit) d  ) locali) ]
	      [(smc env (list a b ...) c (list 'exit d ...) locali)  (exit a)  ]




;		Salvo o ambiente, as localizações atuais e reinicio as localicações
	      [(smc env a b (list (blkComandDec c d) e ...) locali)
	      	(smc env (append (list env locali) a) b (append (list c d 'blk) e)  '()) ]
	      [(smc env a b (list (blkComand c) e ...) locali)
	      	(smc env (append (list env locali) a) b (append (list c 'blk) e)  '()) ]
	      [(smc env (list (? hash? a) (? listLoc? e) b ...) c (list 'blk d ...) locali) (smc a b (clean locali c) d e)]


	      [(smc env a m (list (dec b c) d ...) locali) (smc env a m (append (list b c) d) locali)]
	      [(smc env v m (list (ref a b) r ...) locali) (smc env v m (append (list b 'ref a) r) locali)]
	      [(smc env v m (list (cns a b) r ...) locali) (smc env v m (append (list b 'cns a) r) locali)]
	      [(smc env v m (list (prcFormals (idt i) formals block) r ...) locali)
	      	(smc (constant env i (absFormals formals block)) v m r locali)]
	      [(smc env v m (list (prc (idt i) block) r ...) locali) (smc (constant env i (abs block)) v m r locali)]
	      [(smc env v m (list (cal a) r ...) locali) (smc env v m (append (list a 'cal) r) locali)]
	      [(smc env v m (list (calAtuals id atuals) r ...) locali) (smc env v m (append (list id atuals 'cal) r) locali)]
	      [(smc env (list atuals ... (absFormals formals block) r1 ...) m (list 'cal r ...) locali)
	      	(smc env r1 m (cons (blk (casa formals atuals) block) r) locali) ]
		
	      [(smc env (list (abs block) r1 ...) m (list 'cal r ...) locali)
	      	(smc env r1 m (cons block r) locali) ]
		
	      [(smc env v m (list (act e actuals) r ...) locali) (smc env v m (append (list actuals e) r) locali)]
	      
	      [(smc env (list a v ...) m (list 'ref (idt i) r ...) locali)
	      	(let-values ([(newMem newEnv) (reference env m i a)])
			(smc newEnv v newMem r (cons (hash-ref newEnv i) locali)))]
	      [(smc env (list a v ...) m (list 'cns (idt i) r ...) locali)
	      	(let ([newEnv (constant env i a )]) (smc newEnv v m r locali))]


	      [a (raise (format "Desculpe, feature não implementada. O elemento é ~a\n" a))]))))




