#lang racket

(require "ambiente.rkt")

(struct try/catch (t c) #:transparent)
(struct throw (e) #:transparent)
(struct try/catch/finally (t c f) #:transparent)

;continuation struct, receive environment, value stack and control stack
;invocation of continuation put their fields in correct place, and the value
;that are actual on invocation on top of value stack

(struct cont (env s c) #:transparent)

(struct yield (expression function-name) #:transparent)

(struct ret (a) #:transparent)
(struct calf (a) #:transparent)
(struct calAtualsf (a b) #:transparent)
(struct fun (a b) #:transparent)
(struct funFormals (a b c) #:transparent)
(struct nop () #:transparent)
(struct if-struct (cond then else) #:transparent)
(struct print (a) #:transparent)
(struct exit (a) #:transparent)
(struct act (a b) #:transparent)
(struct calAtuals (a b) #:transparent)
(struct cal (a) #:transparent)
(struct prc (a b) #:transparent)
(struct prcFormals (a b c) #:transparent)
(struct cns (a b) #:transparent)
(struct blkCommand (a) #:transparent)
(struct blkCommandDec (a b) #:transparent)

(struct idt (a) #:transparent)

(struct assign (a b) #:transparent)

(struct loop (a b) #:transparent)

(struct choice (a b) #:transparent)

(struct eq (a b) #:transparent)

(struct neg (a) #:transparent)

(struct ref (a b) #:transparent)
(struct dec (a b) #:transparent)

(struct par (a) #:transparent)
(struct for (a b) #:transparent)

(struct add (a b) #:transparent)
(struct sub (a b) #:transparent)
(struct mult (a b) #:transparent)
(struct div (a b) #:transparent)

(struct or (a b) #:transparent)
(struct and-struct (a b) #:transparent)
(struct ge (a b) #:transparent)
(struct gt (a b) #:transparent)
(struct lt (a b) #:transparent)
(struct le (a b) #:transparent)

(struct seq (a b) #:transparent)

;call/cc receive a 1-arity function, that will be
;invoked now with the actual continuation.
;is important because we can build apart the continuation
;aspect from the process

;in a future implementation of CPS(cyber-physical systems)
;the concept of continuation can change to use continuos time
;but actual code must not be changed because they use call/cc
;to get continuations
;in fact, constructs that use continuations must call call/cc
;with a specialized function.

(struct call/cc (a) #:transparent)

(struct smc (env val mem control loc output) #:transparent)
(struct absFormals (formals block) #:transparent)
(struct abs (block) #:transparent)

(provide executeSMC smc)
(define (executeSMC bplc)
  (smcEval (smc (hash) '() (hash) (list bplc) '() '())))
  
(define (casa formals atuals)
  (match (cons formals atuals)
    [(cons (par a) (list c)) (ref a c)   ]
    [(cons (for a b) (list c d ...)) (dec (ref a c) (casa b d))]))

(define (smcEval smcP)
  (display smcP)
  (newline)
  (if
   (and
    #|(null? (smc-val smcP))|#
    (null? (smc-control smcP)))
   smcP
   (smcEval (match smcP 
              [(smc env d e (list (add a b) c ...) locali output) (smc env d e (append (list a b 'add) c) locali output)]
              [(smc env d e (list (sub a b) c ...) locali output) (smc env d e (append (list a b 'sub) c) locali output)]
              [(smc env d e (list (mult a b) c ...) locali output) (smc env d e (append (list a b 'mult) c) locali output)]
              [(smc env d e (list (div a b) c ...) locali output) (smc env d e (append (list a b 'div) c) locali output)]
              
              [(smc env d e (list (? number? a) b ...) locali output) (smc env (cons a d) e b locali output)]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'add e ...) locali output) (smc env (cons (+ a b) c) d e locali output)]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'sub e ...) locali output) (smc env (cons (- b a) c) d e locali output)]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'mult e ...) locali output) (smc env (cons (* a b) c) d e locali output)]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'div e ...) locali output) (smc env (cons (/ b a) c) d e locali output)]
              
              [(smc env d e (list (? boolean? a) b ...) locali output) (smc env (cons a d) e b  locali output)]
              [(smc env d e (list (? or? a) c ...) locali output) (smc env d e (append (list (or-a a)  (or-b a) 'or) c) locali output) ]
              [(smc env d e (list (? and-struct? a) c ...) locali output) (smc env d e (append (list (and-struct-a a) (and-struct-b a) 'and) c) locali output) ]
              [(smc env d e (list (ge a b) c ...) locali output) (smc env d e (append (list a b 'ge) c) locali output) ]
              [(smc env d e (list (gt a b) c ...) locali output) (smc env d e (append (list a b 'gt) c) locali output) ]
              [(smc env d e (list (lt a b) c ...) locali output) (smc env d e (append (list a b 'lt) c) locali output) ]
              [(smc env d e (list (neg a) c ...) locali output) (smc env d e (append (list a 'neg) c) locali output) ]
              [(smc env d e (list (eq a b) c ...) locali output) (smc env d e (append (list a b 'eq) c) locali output) ]
              [(smc env d e (list (le a b) c ...) locali output) (smc env d e (append (list a b 'le) c) locali output) ]


              [(smc env (list (? boolean? a) (? boolean? b) c ...) d (list 'or e ...) locali output)  (smc env (cons (if a #t b) c) d e locali output) ]
              [(smc env (list (? boolean? a) (? boolean? b) c ...) d (list 'and e ...) locali output)  (smc env (cons (if a b #f) c) d e locali output) ]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'ge e ...) locali output)  (smc env (cons (>= b a) c) d e locali output) ]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'gt e ...) locali output)  (smc env (cons (> b a) c) d e locali output) ]

              [(smc env (list (? number? a) (? number? b) c ...) d (list 'lt e ...) locali output)  (smc env (cons (< b a) c) d e locali output) ]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'eq e ...) locali output)  (smc env (cons (= a b) c) d e locali output) ]
              [(smc env (list (? number? a) (? number? b) c ...) d (list 'le e ...) locali output)  (smc env (cons (<= b a) c) d e locali output) ]
              [(smc env (list (? boolean? a) c ...) d (list 'neg e ...) locali output)  (smc env (cons (not a) c) d e locali output) ]

              [(smc env a d (list (? nop? b) c ...) locali output) (smc env a d c locali output) ]
	      
              [(smc env a d (list (? if-struct? b) c ...) locali output) (smc env a d (append (list (if-struct-cond b) 'if (if-struct-then b) (if-struct-else b)) c) locali output) ]
              [(smc env (list (? boolean? a) b ...) c (list 'if c1 c2 d ...) locali output)  (smc env b c (append (list (if a c1 c2)) d) locali output) ]

              ;		Talvez criar mais um item semantico contendo os efeitos colaterais fosse legal, tornaria o smcEval 100% funcional, sem efeito colateral
              [(smc env a b (list (? print? c) d ...) locali output) (smc env a b (append (list (print-a c) 'print) d) locali output)]
              [(smc env (list a b ...) c (list 'print d ...) locali output) (smc env b c d locali (cons a output) )  ]
	      
              [(smc env a b (list (seq c d) e ...) locali output) (smc env a b (append (list c d) e) locali output) ]


              ;		Poderia mudar isso para uso do operador 'amb', de escolha não-deterministica
              ;		Mas para isso precisaria da chamada de fail em algum lugar, ou seja, precisaria
              ;		de alguma condição sendo testada. Se for uma modal(LTL(linear temporal logic)) pode dar certo
              ;		Obviamente precisaria ser decidivel(não é, então assim, o que eu faço?), mas não sei como fazer com os loops infinitos
              ;		decidiveis
              [(smc env a b (list (choice c d) e ...) locali output)  (smc env a b (append (list (if (equal? 0 (random 2)) c d )) e ) locali output)]

              [(smc env a d (list (loop b e) c ...) locali output) (smc env a d (append (list b 'loop b e) c) locali output) ]

              [(smc env (list (? boolean? a) b ...) c (list 'loop c1 c2 d ...) locali output)  (smc env b c (append (if a (list c2 (loop c1 c2)) '()) d ) locali output) ]
              ;;TODO : verify types in atrib	
              [(smc env a b (list (assign c d) e ...) locali output) (smc env a b (append (list d 'assign c) e) locali output)]
              [(smc env (list a b ...) c (list 'assign (idt d) e ...) locali output) (let ([newMemory (atrib env c d a)]) (smc env b newMemory e locali output))]
	      
              [(smc env a b (list (idt c) d ...) locali output) (let ([v (identifier env b c)]) (smc env (cons v a) b d locali output))]

              [(smc env a b (list (? exit? c) d ...) locali output) (smc env a b (append (list (exit-a c) 'exit) d  ) locali output) ]
              [(smc env (list a b ...) c (list 'exit d ...) locali output)  (exit a)  ]




              ;		Salvo o ambiente, as localizações atuais e reinicio as localicações
              [(smc env a b (list (blkCommandDec c d) e ...) locali output)
               (smc env (append (list env locali) a) b (append (list c d 'blk) e)  '() output) ]
              [(smc env a b (list (blkCommand c) e ...) locali output)
               (smc env (append (list env locali) a) b (append (list c 'blk) e)  '() output) ]
              [(smc env (list (? hash? a) (? listLoc? e) b ...) c (list 'blk d ...) locali output)
               (smc a b c d e output)]


              [(smc env a m (list (dec b c) d ...) locali output) (smc env a m (append (list b c) d) locali output)]
              [(smc env v m (list (ref a b) r ...) locali output) (smc env v m (append (list b 'ref a) r) locali output)]
              [(smc env v m (list (cns a b) r ...) locali output) (smc env v m (append (list b 'cns a) r) locali output)]
              [(smc env v m (list (prcFormals (idt i) formals block) r ...) locali output)
               (smc (constant env i (absFormals formals block)) v m r locali output)]
              [(smc env v m (list (prc (idt i) block) r ...) locali output) (smc (constant env i (abs block)) v m r locali output)]
              [(smc env v m (list (cal a) r ...) locali output) (smc env v m (append (list a 'cal) r) locali output)]
              [(smc env v m (list (calAtuals id atuals) r ...) locali output) (smc env v m (append (list id atuals 'cal) r) locali output)]
              [(smc env (list atual (cont e s c) r ...) m (list 'cal r ...) locali output)
               (smc e (cons atual s) m c locali output)]
              [(smc env (list atuals ... (absFormals formals block) r1 ...) m (list 'cal r ...) locali output)
               (smc env r1 m (cons (blkCommandDec (casa formals atuals) block) r) locali output) ]
		
              [(smc env (list (abs block) r1 ...) m (list 'cal r ...) locali output)
               (smc env r1 m (cons block r) locali output) ]
		
              [(smc env v m (list (act e actuals) r ...) locali output) (smc env v m (append (list actuals e) r) locali output)]
	      
              [(smc env (list a v ...) m (list 'ref (idt i) r ...) locali output)
               (let-values ([(newMem newEnv) (reference env m i a)])
                 (smc newEnv v newMem r (cons (hash-ref newEnv i) locali) output))]
              [(smc env (list a v ...) m (list 'cns (idt i) r ...) locali output)
               (let ([newEnv (constant env i a )]) (smc newEnv v m r locali output))]

               [(smc env v m (list (calf a) r ...) locali output)
              	(smc env (cons (cont env v r) v) m (append (list a 'cal) r) locali output)]
              [(smc env v m (list (calAtualsf id atuals) r ...) locali output)
              (smc env (cons (cont env v r) v) m (append (list id atuals 'cal) r) locali output)]
	      
               [(smc env v m (list (funFormals (idt i) formals block) r ...) locali output)
              	(smc (constant env i (absFormals formals block)) v m r locali output)]
              [(smc env v m (list (fun (idt i) block) r ...) locali output) (smc (constant env i (abs block)) v m r locali output)]
	      
              [(smc env a m (list (ret c) d ...) locali output) (smc env a m (append (list c 'ret) d) locali output)]
              [(smc env (list v a ... (cont e s c) r ...) m (list 'ret r ...) locali output)
               (smc e (cons v s) m c locali output)]

              [(smc env v m (list (call/cc f) c ...) locali output)
               (smc env v m (cons (calf f (cont env v c)) c) locali output) ]
              [(smc env v m (list (? cont? a) c ...) locali output) (smc env (cons a v) m c locali output)]

	      ;;;Como fazer isso respeitando a tipagem??

	      [(smc env v m (list (yield e (idt i)) c ...) locali output)
	       (let ([env2 (constant env i
				     (call/cc
				       (absFormals (par (idt "k"))
						   (cal
						     (cont
						       env
						       v
						       c) (idt "k")))))])
		 (smc env2 v m (cons (ret e) c) locali output))]

	      [(smc env v m (list (try/catch e c) r ...) locali output) (smc env v m (cons (try/catch/finally e c (nop)) r) locali output)]
		;;;Possivelmente essa ultima transição virará um throw
              [a (raise (format "Desculpe, feature não implementada. O elemento é ~a\n" a))]))))


(module+ test
  (require rackunit)
  
  (check-equal? (executeSMC (add 1 2))
                (smc (hash) '(3) (hash) '() '() '()))
  (check-equal? (executeSMC (sub 1 2))
                (smc (hash) '(-1) (hash) '() '() '()))
  (check-equal? (executeSMC (mult 1 2))
                (smc (hash) '(2) (hash) '() '() '()))
  (check-equal? (executeSMC (div 1 2))
                (smc (hash) (list (/ 1 2)) (hash) '() '() '()))
  (check-equal? (executeSMC 2)
                (smc (hash) (list 2) (hash) '() '() '()))


  (check-equal? (executeSMC #t)
                (smc (hash) (list #t) (hash) '() '() '()))
  (check-equal? (executeSMC (or #t #f))
                (smc (hash) (list #t) (hash) '() '() '()))
  (check-equal? (executeSMC (and-struct #t #f))
                (smc (hash) (list #f) (hash) '() '() '()))
  (check-equal? (executeSMC (ge 42 67))
                (smc (hash) (list #f) (hash) '() '() '()))
  (check-equal? (executeSMC (gt 42 67))
                (smc (hash) (list #f) (hash) '() '() '()))
  (check-equal? (executeSMC (eq 42 67))
                (smc (hash) (list #f) (hash) '() '() '()))
  (check-equal? (executeSMC (lt 42 67))
                (smc (hash) (list #t) (hash) '() '() '()))
  (check-equal? (executeSMC (le 42 67))
                (smc (hash) (list #t) (hash) '() '() '()))
  (check-equal? (executeSMC (neg #t))
                (smc (hash) (list #f) (hash) '() '() '()))
  (check-equal? (executeSMC (nop))
                (smc (hash) (list) (hash) '() '() '()))



  (check-equal? (executeSMC (if-struct (lt 4 5) (add 1 2) (sub 1 2)))
                (smc (hash) (list 3) (hash) '() '() '()))
  (check-equal? (executeSMC (print 4))
                (smc (hash) (list) (hash) '() '() '(4)))
  (check-equal? (executeSMC (seq (print 3) (print 2)))
                (smc (hash) (list) (hash) '()'() '(2 3)))
  (check-equal? (executeSMC (ref (idt "x") 4))
                (smc (hash "x" (loc 1)) '() (hash (loc 1) 4) '() (list (loc 1)) '()))

  (check-equal? (executeSMC (seq (ref (idt "x") 0) (assign (idt "x") 4)))
                (smc (hash "x" (loc 1)) '() (hash (loc 1) 4) '() (list (loc 1)) '()))

  (check-equal? (executeSMC (seq (ref (idt "x") 0) (loop (lt (idt "x") 5) (blkCommand (seq (print (idt "x")) (assign (idt "x") (add (idt "x") 1)))))))
                (smc (hash "x" (loc 1)) '() (hash (loc 1) 5) '() (list (loc 1)) '(4 3 2 1 0))))