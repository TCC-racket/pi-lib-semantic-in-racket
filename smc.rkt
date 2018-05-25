#lang racket
; smc (hash? list? hash? list? (listof loc))
(struct smc (env val mem control loc) #:transparent)
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
  (smcEval (smc (hash) '() (hash) (list bplc) '())))
  
(define (smcEval smcP)
  (writeln smcP)
;  (sleep 1)
  (match smcP [(smc env (list) a (list) locali) (smc env (list) a (list) locali) ]
	      [(smc env d e (list (add a b) c ...) locali) (smcEval (smc env d e (append (list a b 'add) c) locali))]
              [(smc env d e (list (sub a b) c ...) locali) (smcEval (smc env d e (append (list a b 'sub) c) locali))]
              [(smc env d e (list (mult a b) c ...) locali) (smcEval (smc env d e (append (list a b 'mult) c) locali))]
              [(smc env d e (list (div a b) c ...) locali) (smcEval (smc env d e (append (list a b 'div) c) locali))]
              
              [(smc env d e (list (? number? a) b ...) locali) (smcEval (smc env (cons a d) e b locali))]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'add e ...) locali) (smcEval (smc env (cons (+ a b) c) d e locali))]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'sub e ...) locali) (smcEval (smc env (cons (- b a) c) d e locali))]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'mult e ...) locali) (smcEval (smc env (cons (* a b) c) d e locali))]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'div e ...) locali) (smcEval (smc env (cons (/ b a) c) d e locali))]
              
              [(smc env d e (list (? boolean? a) b ...) locali) (smcEval (smc env (cons a d) e b  locali))]
              [(smc env d e (list (? or? a) c ...) locali) (smcEval (smc env d e (append (list (or-a a)  (or-b a) 'or) c) locali)) ]
	      [(smc env d e (list (? and? a) c ...) locali) (smcEval (smc env d e (append (list (and-a a) (and-b a) 'and) c) locali)) ]
	      [(smc env d e (list (ge a b) c ...) locali) (smcEval (smc env d e (append (list a b 'ge) c) locali)) ]
	      [(smc env d e (list (gt a b) c ...) locali) (smcEval (smc env d e (append (list a b 'gt) c) locali)) ]
	      [(smc env d e (list (lt a b) c ...) locali) (smcEval (smc env d e (append (list a b 'lt) c) locali)) ]
	      [(smc env d e (list (neg a) c ...) locali) (smcEval (smc env d e (append (list a 'neg) c) locali)) ]
	      [(smc env d e (list (eq a b) c ...) locali) (smcEval (smc env d e (append (list a b 'eq) c) locali)) ]
	      [(smc env d e (list (le a b) c ...) locali) (smcEval (smc env d e (append (list a b 'le) c) locali)) ]


	      [(smc env (list (? boolean? a) (? boolean? b) c ...) d (list 'or e ...) locali)  (smcEval (smc env (cons (if a #t b) c) d e locali)) ]
	      [(smc env (list (? boolean? a) (? boolean? b) c ...) d (list 'and e ...) locali)  (smcEval (smc env (cons (if a b #f) c) d e locali)) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'ge e ...) locali)  (smcEval (smc env (cons (>= b a) c) d e locali)) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'gt e ...) locali)  (smcEval (smc env (cons (> b a) c) d e locali)) ]

	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'lt e ...) locali)  (smcEval (smc env (cons (< b a) c) d e locali)) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'eq e ...) locali)  (smcEval (smc env (cons (= a b) c) d e) locali) ]
	      [(smc env (list (? number? a) (? number? b) c ...) d (list 'le e ...) locali)  (smcEval (smc env (cons (<= b a) c) d e) locali) ]
	      [(smc env (list (? boolean? a) c ...) d (list 'neg e ...) locali)  (smcEval (smc env (cons (not a) c) d e) locali) ]


;		possivel adição ao IMP = strings
;	      [(smc env a b (list (? string? c) d ...)) (smcEval (smc env (cons c a) b d))    ]
	      [(smc env a d (list (? nop? b) c ...) locali) (smcEval (smc env a d c locali)) ]
	      [(smc env a d (list (? if? b) c ...) locali) (smcEval (smc env a d (append (list (if-cond b) 'if (if-then b) (if-else b)) c) locali)) ]
;		Isso não é C, if testa apenas booleanos
;	      [(smc env (list (? number? a) b ...) c (list 'if c1 c2 d ...))  (smcEval (smc env b c (append (list (if (not (equal? a 0)) c1 c2)) d))) ]
	      [(smc env (list (? boolean? a) b ...) c (list 'if c1 c2 d ...) locali)  (smcEval (smc env b c (append (list (if a c1 c2)) d) locali)) ]

;		Talvez criar mais um item semantico contendo os efeitos colaterais fosse legal, tornaria o smcEval 100% funcional, sem efeito colateral
	      [(smc env a b (list (? print? c) d ...) locali) (smcEval (smc env a b (append (list (print-a c) 'print) d) locali) )]
	      [(smc env (list a b ...) c (list 'print d ...) locali) (begin (display a) (smcEval (smc env b c d locali) ))  ]
	      [(smc env a b (list (seq c d) e ...) locali) (smcEval (smc env a b (append (list c d) e) locali)) ]


;		Poderia mudar isso para uso do operador 'amb', de escolha não-deterministica
;		Mas para isso precisaria da chamada de fail em algum lugar, ou seja, precisaria
;		de alguma condição sendo testada. Se for uma modal(talvez PDL??) pode dar certo
;		Obviamente precisaria ser decidivel, mas não sei como fazer com os loops infinitos
;		decidiveis
	      [(smc env a b (list (choice c d) e ...) locali)  (smcEval (smc env a b (append (list (if (equal? 0 (random 2)) c d )) e ) locali))]
	      [(smc env a d (list (loop b e) c ...) locali) (smcEval (smc env a d (append (list b 'loop b e) c) locali)) ]

;		Isso não é C, loop testa só booleanos
;	      [(smc env (list (? number? a) b ...) c (list 'loop c1 c2 d ...))  (smcEval (smc env b c (append (if (not (equal? a 0)) (list c2 (loop c1 c2)) '()) d ))) ]
	      [(smc env (list (? boolean? a) b ...) c (list 'loop c1 c2 d ...) locali)  (smcEval (smc env b c (append (if a (list c2 (loop c1 c2)) '()) d ) locali)) ]
	
	      [(smc env a b (list (assign c d) e ...) locali) (smcEval (smc env a b (append (list d 'assign c) e) locali))]
	      [(smc env (list a b ...) c (list 'assign (idt d) e ...) locali) (let ([newMemory (atrib env c d a)]) (smcEval (smc env b newMemory e locali)))]
	      [(smc env a b (list (idt c) d ...) locali) (let ([v (identifier env b c)]) (smcEval (smc env (cons v a) b d locali)))]

	      [(smc env a b (list (? exit? c) d ...) locali) (smcEval (smc env a b (append (list (exit-a c) 'exit) d  ) locali)) ]
	      [(smc env (list a b ...) c (list 'exit d ...) locali)  (exit a)  ]




;		Salvo o ambiente, as localizações atuais e reinicio as localicações
	      [(smc env a b (list (blk c d) e ...) locali) (smcEval (smc env (append (list env locali) a) b (append (list c d 'blk) e)  '())) ]


	      [(smc env (list a e b ...) c (list 'blk d ...) locali) (smcEval (smc a b (clean locali c) d e))]
	      [(smc env a m (list (dec a b) c ...) locali) (smcEval (smc env a m (append (list a b) c) locali))]
	      [(smc env v m (list (ref a b) r ...) locali) (smcEval (smc env v m (append (list b 'ref a) r) locali))]
	      [(smc env v m (list (cns a b) r ...) locali) (smcEval (smc env v m (append (list b 'cns a) r) locali))]
	      [(smc env (list a v ...) m (list 'ref (idt i) r ...) locali) (let-values ([(newMem newEnv) (reference env m i a)]) (smcEval (smc newEnv v newMem r (cons (hash-ref newEnv i) r)))]
	      [(smc env (list a v ...) m (list 'cns (idt i) r ...) locali) (let ([newEnv (constant env i a )]) (smcEval (smc newEnv v m r locali)))]


	      [a (raise (format "Desculpe, feature não implementada. O elemento é ~a\n" a))]))




