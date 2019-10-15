#lang racket


(provide (all-defined-out))
(require "ambiente.rkt")
(require "pretty.rkt")

(my-struct try/catch (t c) #:transparent)
(my-struct throw (e) #:transparent)
(my-struct try/catch/finally (t c f) #:transparent)

(my-struct cont (env s c) #:transparent)


(my-struct yield (expression function-name) #:transparent)

(my-struct ret (a) #:transparent)

(my-struct calf (a) #:transparent)

(my-struct calAtualsf (a b) #:transparent)

(my-struct fun (a b) #:transparent)
(my-struct funFormals (a b c) #:transparent)
(my-struct nop () #:transparent)
(my-struct if-my-struct (cond then else) #:transparent)
(my-struct print (a) #:transparent)
(my-struct exit (a) #:transparent)
(my-struct act (a b) #:transparent)
(my-struct calAtuals (a b) #:transparent)
(my-struct cal (a) #:transparent)
(my-struct prc (a b) #:transparent)
(my-struct prcFormals (a b c) #:transparent)
(my-struct cns (a b) #:transparent)
(my-struct blkCommand (a) #:transparent)
(my-struct blkCommandDec (a b) #:transparent)

(my-struct idt (a) #:transparent)

(my-struct assign (a b) #:transparent)

(my-struct loop (a b) #:transparent)

(my-struct choice (a b) #:transparent)

(my-struct eq (a b) #:transparent)

(my-struct neg (a) #:transparent)

(my-struct ref (a) #:transparent)
(my-struct dec (a b) #:transparent)

(my-struct par (a) #:transparent)
(my-struct for (a b) #:transparent)

(my-struct add (a b) #:transparent)
(my-struct sub (a b) #:transparent)
(my-struct mult (a b) #:transparent)
(my-struct div (a b) #:transparent)

(my-struct or (a b) #:transparent)
(my-struct and-my-struct (a b) #:transparent)
(my-struct ge (a b) #:transparent)
(my-struct gt (a b) #:transparent)
(my-struct lt (a b) #:transparent)
(my-struct le (a b) #:transparent)

(my-struct seq (a b) #:transparent)

(my-struct call/cc (a) #:transparent)

(my-struct piAutomata (env val mem control loc output) #:transparent)
(my-struct absFormals (formals block) #:transparent)
(my-struct abs (block) #:transparent)
(my-struct bind (a b) #:transparent)

(provide executeSMC piAutomata)
(define (executeSMC bplc)
  (piAutomataEval (piAutomata (hash) '() (hash) (list bplc) '() '())))
  
(define (casa formals atuals)
  (match (cons formals atuals)
    [(cons (par a) (list c)) (bind a (ref c))   ]
    [(cons (for a b) (list c d ...)) (dec (bind a (ref c)) (casa b d))]))

(define (piAutomataEval piAutomataP)
  
  (display piAutomataP)
  (newline)

  (if
   (and
    #|(null? (piAutomata-val piAutomataP))|#
    (null? (piAutomata-control piAutomataP)))
   piAutomataP
   (begin
     (display "=")
     (newline)
     (piAutomataEval (match piAutomataP
              [(piAutomata env d e (list (abs b) c ...) locali output) (piAutomata env (cons (abs b) d) e c locali output)]
              [(piAutomata env d e (list (absFormals a b) c ...) locali output) (piAutomata env (cons (absFormals a b) d) e c locali output)]
              [(piAutomata env d e (list (bind a b) c ...) locali output) (piAutomata env (cons a d) e (append (list b 'bind) c) locali output)]
              [(piAutomata env (list (loc l) (idt x) d ...) e (list 'bind c ...) locali output)
               (piAutomata (hash-set env x (loc l)) d e c locali output)]
              [(piAutomata env d e (list (add a b) c ...) locali output) (piAutomata env d e (append (list a b 'add) c) locali output)]
              [(piAutomata env d e (list (sub a b) c ...) locali output) (piAutomata env d e (append (list a b 'sub) c) locali output)]
              [(piAutomata env d e (list (mult a b) c ...) locali output) (piAutomata env d e (append (list a b 'mult) c) locali output)]
              [(piAutomata env d e (list (div a b) c ...) locali output) (piAutomata env d e (append (list a b 'div) c) locali output)]
              
              [(piAutomata env d e (list (? number? a) b ...) locali output) (piAutomata env (cons a d) e b locali output)]
              [(piAutomata env (list (? number? a) (? number? b) c ...) d (list 'add e ...) locali output) (piAutomata env (cons (+ a b) c) d e locali output)]
              [(piAutomata env (list (? number? a) (? number? b) c ...) d (list 'sub e ...) locali output) (piAutomata env (cons (- b a) c) d e locali output)]
              [(piAutomata env (list (? number? a) (? number? b) c ...) d (list 'mult e ...) locali output) (piAutomata env (cons (* a b) c) d e locali output)]
              [(piAutomata env (list (? number? a) (? number? b) c ...) d (list 'div e ...) locali output) (piAutomata env (cons (/ b a) c) d e locali output)]
              
              [(piAutomata env d e (list (? boolean? a) b ...) locali output) (piAutomata env (cons a d) e b  locali output)]
              [(piAutomata env d e (list (? or? a) c ...) locali output) (piAutomata env d e (append (list (or-a a)  (or-b a) 'or) c) locali output) ]
              [(piAutomata env d e (list (? and-my-struct? a) c ...) locali output) (piAutomata env d e (append (list (and-my-struct-a a) (and-my-struct-b a) 'and) c) locali output) ]
              [(piAutomata env d e (list (ge a b) c ...) locali output) (piAutomata env d e (append (list a b 'ge) c) locali output) ]
              [(piAutomata env d e (list (gt a b) c ...) locali output) (piAutomata env d e (append (list a b 'gt) c) locali output) ]
              [(piAutomata env d e (list (lt a b) c ...) locali output) (piAutomata env d e (append (list a b 'lt) c) locali output) ]
              [(piAutomata env d e (list (neg a) c ...) locali output) (piAutomata env d e (append (list a 'neg) c) locali output) ]
              [(piAutomata env d e (list (eq a b) c ...) locali output) (piAutomata env d e (append (list a b 'eq) c) locali output) ]
              [(piAutomata env d e (list (le a b) c ...) locali output) (piAutomata env d e (append (list a b 'le) c) locali output) ]


              [(piAutomata env (list (? boolean? a) (? boolean? b) c ...) d (list 'or e ...) locali output)  (piAutomata env (cons (if a #t b) c) d e locali output) ]
              [(piAutomata env (list (? boolean? a) (? boolean? b) c ...) d (list 'and e ...) locali output)  (piAutomata env (cons (if a b #f) c) d e locali output) ]
              [(piAutomata env (list (? number? a) (? number? b) c ...) d (list 'ge e ...) locali output)  (piAutomata env (cons (>= b a) c) d e locali output) ]
              [(piAutomata env (list (? number? a) (? number? b) c ...) d (list 'gt e ...) locali output)  (piAutomata env (cons (> b a) c) d e locali output) ]

              [(piAutomata env (list (? number? a) (? number? b) c ...) d (list 'lt e ...) locali output)  (piAutomata env (cons (< b a) c) d e locali output) ]
              [(piAutomata env (list (? number? a) (? number? b) c ...) d (list 'eq e ...) locali output)  (piAutomata env (cons (= a b) c) d e locali output) ]
              [(piAutomata env (list (? number? a) (? number? b) c ...) d (list 'le e ...) locali output)  (piAutomata env (cons (<= b a) c) d e locali output) ]
              [(piAutomata env (list (? boolean? a) c ...) d (list 'neg e ...) locali output)  (piAutomata env (cons (not a) c) d e locali output) ]

              [(piAutomata env a d (list (? nop? b) c ...) locali output) (piAutomata env a d c locali output) ]
	      
              [(piAutomata env a d (list (? if-my-struct? b) c ...) locali output) (piAutomata env a d (append (list (if-my-struct-cond b) 'if (if-my-struct-then b) (if-my-struct-else b)) c) locali output) ]
              [(piAutomata env (list (? boolean? a) b ...) c (list 'if c1 c2 d ...) locali output)  (piAutomata env b c (append (list (if a c1 c2)) d) locali output) ]

              ;		Talvez criar mais um item semantico contendo os efeitos colaterais fosse legal, tornaria o piAutomataEval 100% funcional, sem efeito colateral
              [(piAutomata env a b (list (? print? c) d ...) locali output) (piAutomata env a b (append (list (print-a c) 'print) d) locali output)]
              [(piAutomata env (list a b ...) c (list 'print d ...) locali output) (piAutomata env b c d locali (cons a output) )  ]
	      
              [(piAutomata env a b (list (seq c d) e ...) locali output) (piAutomata env a b (append (list c d) e) locali output) ]


              ;		Poderia mudar isso para uso do operador 'amb', de escolha não-deterministica
              ;		Mas para isso precisaria da chamada de fail em algum lugar, ou seja, precisaria
              ;		de alguma condição sendo testada. Se for uma modal(LTL(linear temporal logic)) pode dar certo
              ;		Obviamente precisaria ser decidivel(não é, então assim, o que eu faço?), mas não sei como fazer com os loops infinitos
              ;		decidiveis
              [(piAutomata env a b (list (choice c d) e ...) locali output)  (piAutomata env a b (append (list (if (equal? 0 (random 2)) c d )) e ) locali output)]

              [(piAutomata env a d (list (loop b e) c ...) locali output) (piAutomata env a d (append (list b 'loop b e) c) locali output) ]

              [(piAutomata env (list (? boolean? a) b ...) c (list 'loop c1 c2 d ...) locali output)  (piAutomata env b c (append (if a (list c2 (loop c1 c2)) '()) d ) locali output) ]
              ;;TODO : verify types in atrib	
              [(piAutomata env a b (list (assign c d) e ...) locali output) (piAutomata env a b (append (list d 'assign c) e) locali output)]
              [(piAutomata env (list a b ...) c (list 'assign (idt d) e ...) locali output) (let ([newMemory (atrib env c d a)]) (piAutomata env b newMemory e locali output))]
	      
              [(piAutomata env a b (list (idt c) d ...) locali output) (let ([v (identifier env b c)]) (piAutomata env (cons v a) b d locali output))]

              [(piAutomata env a b (list (? exit? c) d ...) locali output) (piAutomata env a b (append (list (exit-a c) 'exit) d  ) locali output) ]
              [(piAutomata env (list a b ...) c (list 'exit d ...) locali output)  (exit a)  ]




              ;		Salvo o ambiente, as localizações atuais e reinicio as localicações
              [(piAutomata env a b (list (blkCommandDec c d) e ...) locali output)
               (piAutomata env (append (list env locali) a) b (append (list c d 'blk) e)  '() output) ]
              [(piAutomata env a b (list (blkCommand c) e ...) locali output)
               (piAutomata env (append (list env locali) a) b (append (list c 'blk) e)  '() output) ]
              [(piAutomata env (list (? hash? a) (? listLoc? e) b ...) c (list 'blk d ...) locali output)
               (piAutomata a b c d e output)]


              [(piAutomata env a m (list (dec b c) d ...) locali output) (piAutomata env a m (append (list b c) d) locali output)]
              [(piAutomata env v m (list (ref a) r ...) locali output) (piAutomata env v m (append (list a 'ref) r) locali output)]
              [(piAutomata env v m (list (cns a b) r ...) locali output) (piAutomata env v m (append (list b 'cns a) r) locali output)]
              [(piAutomata env v m (list (prcFormals (idt i) formals block) r ...) locali output)
               (piAutomata (constant env i (absFormals formals block)) v m r locali output)]
              [(piAutomata env v m (list (prc (idt i) block) r ...) locali output) (piAutomata (constant env i (abs block)) v m r locali output)]
              [(piAutomata env v m (list (cal a) r ...) locali output)
               (piAutomata env v m (append (list a 'cal) r) locali output)]
              [(piAutomata env v m (list (calAtuals id atuals) r ...) locali output) (piAutomata env v m (append (list id atuals 'cal) r) locali output)]
              [(piAutomata env (list (abs block) r1 ...) m (list 'cal r ...) locali output)
               (piAutomata env r1 m (cons block r) locali output)]
              [(piAutomata env (list atuals ... (absFormals formals block) r1 ...) m (list 'cal r ...) locali output)
               (piAutomata env r1 m (cons (blkCommandDec (casa formals atuals) block) r) locali output) ]
	       ;The invocation of a continuation only means get some value(that can be a number, a boolean
	       ;a abstraction(a function) or another continuation and put her on top of stack value inside cont
	       ;but in particular make output and memory equal.
              [(piAutomata env (list atual (cont e v c) r1 ...) m (list 'cal r ...) locali output)
               (piAutomata e (cons atual v) m c locali output) ]
		
              [(piAutomata env (list (abs block) r1 ...) m (list 'cal r ...) locali output)
               (piAutomata env r1 m (cons block r) locali output) ]
		
              [(piAutomata env v m (list (act e actuals) r ...) locali output) (piAutomata env v m (append (list actuals e) r) locali output)]
	      
              [(piAutomata env (list a v ...) m (list 'ref r ...) locali output)
               (let-values ([(newMem l) (reference m a)])
                 (piAutomata env (cons l v) newMem r (cons l locali) output))]
              [(piAutomata env (list a v ...) m (list 'cns (idt i) r ...) locali output)
               (let ([newEnv (constant env i a )]) (piAutomata newEnv v m r locali output))]

              [(piAutomata env v m (list (calf a) r ...) locali output)
               (piAutomata env (cons (cont env v r) v) m (append (list a 'cal) r) locali output)]
              [(piAutomata env v m (list (calAtualsf id atuals) r ...) locali output)
               (piAutomata env (cons (cont env v r) v) m (append (list id atuals 'cal) r) locali output)]
	      
              [(piAutomata env v m (list (funFormals (idt i) formals block) r ...) locali output)
               (piAutomata (constant env i (absFormals formals block)) v m r locali output)]
              [(piAutomata env v m (list (fun (idt i) block) r ...) locali output) (piAutomata (constant env i (abs block)) v m r locali output)]
	      
	      ;
	      ;As ret is only allowed inside a function(and this must be ensured by parser, because the assumption of
	      ;this semantics is that ret has a continuation(put by a calf or calActualsf),representation of a call to
	      ; a function, somewhere on stack value. 
              [(piAutomata env a m (list (ret c) d ...) locali output) (piAutomata env a m (append (list c 'ret) d) locali output)]
              [(piAutomata env (list v a ... (cont e s c) r1 ...) m (list 'ret r ...) locali output)
               (piAutomata e (cons v s) m c locali output)]

		;
		;call/cc is a special function, in terms of parser, and can be used directly by user or as low level
		;primitive to implementation of coroutines(that will be merged on sunday(9/8/2019) and another high-level
		;points. In fact, there's some points, i.e. function cal, return command and call/cc, that create or consume
		;continuations and that must be changed when the pi-automata increase by for example put cyber-phisical primitives.
		;
              [(piAutomata env v m (list (call/cc f) c ...) locali output)
               (piAutomata env v m (cons (calAtualsf f (cont env v c)) c) locali output) ]
              [(piAutomata env v m (list (? cont? a) c ...) locali output) (piAutomata env (cons a v) m c locali output)]
		
		;
		;Not yet implemented
		;Problaby will be just a smart use of coroutine. When I put this code, I think of doing as a standalone
		;command, but using coroutines look better
              [(piAutomata env v m (list (yield e (idt i)) c ...) locali output)
               (let ([env2 (constant env i
                                     (call/cc
                                      (absFormals (par (idt "k"))
                                                  (cal
                                                   (cont
                                                    env
                                                    v
                                                    c) (idt "k")))))])
                 (piAutomata env2 v m (cons (ret e) c) locali output))]

		;;
		;;Will be merged as soon as possible. Exceptions are just out-going only continuations, but delimited continuations
		;;is better to her. Maybe I just put amb and coroutines merged and add try/catch changed with delimited in the future.
              [(piAutomata env v m (list (try/catch e c) r ...) locali output) (piAutomata env v m (cons (try/catch/finally e c (nop)) r) locali output)]
              ;;;Possivelmente essa ultima transição virará um throw
              [a (begin
                   (raise (format "Desculpe, feature não implementada.\n")))])))))


(module+ test
;;rackunit is a unit test suite of racket
  (require rackunit)
;;check-equal? receive 2 arguments and raise a exception if (equal? a b) return #f

;;
;;
;;The correct was only stop recursion of SMCEval with both control and value stack empty
;;but to allow test of arithmetic expression, including the rules that look to cont as a value
;;this rule was relaxed, only control must be empty
;;
  
  (check-equal? (executeSMC (add 1 2))
                (piAutomata (hash) '(3) (hash) '() '() '()))
  (check-equal? (executeSMC (sub 1 2))
                (piAutomata (hash) '(-1) (hash) '() '() '()))
  (check-equal? (executeSMC (mult 1 2))
                (piAutomata (hash) '(2) (hash) '() '() '()))
  (check-equal? (executeSMC (div 1 2))
                (piAutomata (hash) (list (/ 1 2)) (hash) '() '() '()))
  (check-equal? (executeSMC 2)
                (piAutomata (hash) (list 2) (hash) '() '() '()))


  (check-equal? (executeSMC #t)
                (piAutomata (hash) (list #t) (hash) '() '() '()))
  (check-equal? (executeSMC (or #t #f))
                (piAutomata (hash) (list #t) (hash) '() '() '()))
  (check-equal? (executeSMC (and-my-struct #t #f))
                (piAutomata (hash) (list #f) (hash) '() '() '()))
  (check-equal? (executeSMC (ge 42 67))
                (piAutomata (hash) (list #f) (hash) '() '() '()))
  (check-equal? (executeSMC (gt 42 67))
                (piAutomata (hash) (list #f) (hash) '() '() '()))
  (check-equal? (executeSMC (eq 42 67))
                (piAutomata (hash) (list #f) (hash) '() '() '()))
  (check-equal? (executeSMC (lt 42 67))
                (piAutomata (hash) (list #t) (hash) '() '() '()))
  (check-equal? (executeSMC (le 42 67))
                (piAutomata (hash) (list #t) (hash) '() '() '()))
  (check-equal? (executeSMC (neg #t))
                (piAutomata (hash) (list #f) (hash) '() '() '()))
  (check-equal? (executeSMC (nop))
                (piAutomata (hash) (list) (hash) '() '() '()))



  (check-equal? (executeSMC (if-my-struct (lt 4 5) (add 1 2) (sub 1 2)))
                (piAutomata (hash) (list 3) (hash) '() '() '()))
  (check-equal? (executeSMC (print 4))
                (piAutomata (hash) (list) (hash) '() '() '(4)))
  (check-equal? (executeSMC (seq (print 3) (print 2)))
                (piAutomata (hash) (list) (hash) '()'() '(2 3)))
  (check-equal? (executeSMC (bind (idt "x") (ref 4)))
                (piAutomata (hash "x" (loc 1)) '() (hash (loc 1) 4) '() (list (loc 1)) '()))

  (check-equal? (executeSMC
                 (seq
                  (bind (idt "x") (ref 0))
                  (assign (idt "x") 4)))
                (piAutomata (hash "x" (loc 1)) '() (hash (loc 1) 4) '() (list (loc 1)) '()))

  (check-equal? (executeSMC
                 (seq (bind (idt "x") (ref 0))
                      (loop
                       (lt (idt "x") 5)
                       (blkCommand
                        (seq (print (idt "x"))
                             (assign (idt "x") (add (idt "x") 1)))))))
                (piAutomata (hash "x" (loc 1)) '() (hash (loc 1) 5) '() (list (loc 1)) '(4 3 2 1 0)))

  (check-equal?
   (executeSMC
    (prcFormals (idt "f") (par (idt "k"))
                (blkCommand (ret 2))))
   (piAutomata (hash "f" (absFormals (par (idt "k")) (blkCommand (ret 2)))) '() '#hash() '() '() '()))

;;Here we can see the code
;;
;;
;;fun f(k)
;;{
;;print(2);
;;}
;;f(1);
;;
;;be called
  (check-equal?
   (executeSMC
    (blkCommandDec
     (prcFormals
      (idt "f")
      (par (idt "k"))
      (blkCommand (print 2)))
     (calAtuals
      (idt "f")
      1)))
   (piAutomata '#hash() '() (hash (loc 1) 1) '() '() '(2)))

  ;(my-my-struct piAutomata (env val mem control loc output) #:transparent)
  ;(my-my-struct cont (env s c) #:transparent)
  (check-equal?
   (piAutomataEval
    (piAutomata (hash) (list 2 (cont (hash) '() (list 'print))) (hash) (list 'ret) '() '()))
   (piAutomata (hash) '() (hash) '() '() (list 2)))

  (check-equal?
   (piAutomataEval
    (piAutomata (hash) (list 2 3 4 5 (cont (hash) '() (list 'print))) (hash) (list 'ret) '() '()))
   (piAutomata (hash) '() (hash) '() '() (list 2)))
  
  (check-equal?
   (piAutomataEval
    (piAutomata (hash) (list 2 3 4 5 (cont (hash) '() (list 'print))) (hash) (list (ret 2)) '() '()))
   (piAutomata (hash) '() (hash) '() '() (list 2)))

  ;
  ;
  ;
  ;
  ;

  (check-equal?
   (executeSMC (funFormals
                (idt "f")
                (par (idt "k"))
                (blkCommand (ret 2))))
   (piAutomata (hash "f" (absFormals (par (idt "k")) (blkCommand (ret 2)))) '() '#hash() '() '() '()))

  (check-equal?
   (executeSMC
    (blkCommandDec
     (funFormals
      (idt "f")
      (par (idt "k"))
      (blkCommand (ret 2)))
     (nop)))
   (piAutomata '#hash() '() (hash) '() '() '()))

  (check-equal?
   (piAutomataEval
    (piAutomata (hash "f" (absFormals (par (idt "k")) (blkCommand (ret 2)))) '() (hash) (list (print 2)) '() '()))
   (piAutomata (hash "f" (absFormals (par (idt "k")) (blkCommand (ret 2)))) '() (hash) '() '() (list 2)))
  
  (check-equal?
   (piAutomataEval
    (piAutomata (hash "f" (abs (blkCommand (ret 2)))) '() (hash) (list (calf (idt "f"))) '() '()))
   (piAutomata (hash "f" (abs (blkCommand (ret 2)))) '(2) (hash) '() '() '()))
  
  (check-equal?
   (piAutomataEval
    (piAutomata (hash "f" (abs (blkCommand (ret 2)))) '() (hash) (list (print (calf (idt "f")))) '() '()))
   (piAutomata (hash "f" (abs (blkCommand (ret 2)))) '() (hash) '() '() '(2)))

  (check-equal?
   (executeSMC
    (blkCommandDec
     (funFormals
      (idt "f")
      (par (idt "k"))
      (blkCommand (ret 2)))
     (print (calAtualsf (idt "f") 1))))
   (piAutomata '#hash() '() (hash (loc 1) 1) '() '() '(2)))

  (check-equal?
   (executeSMC
    (calAtualsf (cont (hash "f" (loc 1) "x" (loc 2)) '(2) '()) 21))
   (piAutomata (hash "f" (loc 1) "x" (loc 2)) (list 21 2) (hash) '() '() '()))
  
  (check-equal?
   (executeSMC
    (blkCommandDec
     (funFormals
      (idt "f")
      (par (idt "k"))
      (blkCommand (calAtualsf (idt "k") 42)))
     (calAtualsf (idt "f") (cont (hash 1 2 3 4) '(45) '()))))
   (piAutomata (hash 1 2 3 4) (list 42 45) (hash (loc 1) (cont '#hash((1 . 2) (3 . 4)) '(45) '())) '() '() '()))

;
;;
;
;
;outerK := #f;
;fun f(k)
;{
;
;outerK=k;
;return 0;
;
;}
;
;x := call/cc(f);
;if(x<5)
;{
;
;print(x);
;outerK(1+x);
;
;}
;
;
;
;There's a lot of locations on env because
;instead of inside a assign, call/cc is inside a ref
;so when outerK is called, a new location on x is created
;
;if was a assign, this don't happened.
;
;
;
  (check-equal?
   (executeSMC
    (blkCommandDec
     (dec
      (bind (idt "outerK") (ref #f))
      (funFormals
       (idt "f")
       (par (idt "k"))
       (blkCommand (seq
                    (assign (idt "outerK")(idt "k"))
                    (ret 0)))))
     (blkCommandDec
      (bind (idt "x") (ref (call/cc (idt "f"))))
      (if-my-struct (lt (idt "x") 5)
                 (seq
                  (print (idt "x"))
                  (calAtualsf (idt "outerK") (add 1 (idt "x"))))
                 (nop)))))
   (piAutomata
 '#hash()
 '()
 (hash
  (loc 2)
  (cont
   (hash "f" (absFormals (par (idt "k")) (blkCommand (seq (assign (idt "outerK") (idt "k")) (ret 0)))) "outerK" (loc 1))
   (list
    (idt "x")
    (hash "f" (absFormals (par (idt "k")) (blkCommand (seq (assign (idt "outerK") (idt "k")) (ret 0)))) "outerK" (loc 1))
    (list (loc 1))
    '#hash()
    '())
   (list
    'ref
    'bind
    (if-my-struct (lt (idt "x") 5) (seq (print (idt "x")) (calAtualsf (idt "outerK") (add 1 (idt "x")))) (nop))
    'blk
    'blk))
  (loc 5)
  2
  (loc 3)
  0
  (loc 7)
  4
  (loc 8)
  5
  (loc 6)
  3
  (loc 4)
  1
  (loc 1)
  (cont
   (hash "f" (absFormals (par (idt "k")) (blkCommand (seq (assign (idt "outerK") (idt "k")) (ret 0)))) "outerK" (loc 1))
   (list
    (idt "x")
    (hash "f" (absFormals (par (idt "k")) (blkCommand (seq (assign (idt "outerK") (idt "k")) (ret 0)))) "outerK" (loc 1))
    (list (loc 1))
    '#hash()
    '())
   (list
    'ref
    'bind
    (if-my-struct (lt (idt "x") 5) (seq (print (idt "x")) (calAtualsf (idt "outerK") (add 1 (idt "x")))) (nop))
    'blk
    'blk)))
 '()
 '()
 '(4 3 2 1 0)))


  (check-equal?
   (executeSMC
    (blkCommandDec
     (dec
      (bind (idt "resource") (ref 0))
      (dec
       (funFormals
        (idt "produtor")
        (par (idt "k1"))
        (blkCommandDec
         (bind (idt "co") (ref 5))
         (seq
          (loop (gt (idt "co") 0)
                (seq
                 (assign (idt "co") (sub (idt "co") 1))
                 (seq
                  (assign (idt "resource") 2)
                  (assign (idt "k1") (call/cc (idt "k1"))))))
          (seq
           (assign (idt "resource") 0)
           (ret 0)))))
       (funFormals
        (idt "consumidor")
        (par (idt "k2"))
        (blkCommandDec
         (bind (idt "i") (ref 0))
         (seq
                     (if-my-struct (lt (idt "resource") 1)
                                    (assign (idt "k2") (call/cc (idt "k2")))
                                    (nop))
                     (seq
                     (loop
                      (gt (idt "resource") 0)
                      (seq
                       (print (idt "resource"))
                       (seq
                        (assign (idt "resource") (sub (idt "resource") 1))
                        (if-my-struct
                         (eq (idt "resource") 0)
                            (assign (idt "k2") (call/cc (idt "k2")))
                            (nop)))))
                     (ret 0)))))))
     (print (calAtualsf (idt "consumidor") (idt "produtor")))))
   (piAutomata
 '#hash()
 '()
 (hash
  (loc 2)
  (cont
   (hash
    "co"
    (loc 5)
    "consumidor"
    (absFormals
     (par (idt "k2"))
     (blkCommandDec
      (bind (idt "i") (ref 0))
      (seq
       (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
       (seq
        (loop
         (gt (idt "resource") 0)
         (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
        (ret 0)))))
    "i"
    (loc 3)
    "k1"
    (loc 4)
    "k2"
    (loc 2)
    "produtor"
    (absFormals
     (par (idt "k1"))
     (blkCommandDec
      (bind (idt "co") (ref 5))
      (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
    "resource"
    (loc 1))
   (list
    (hash
     "consumidor"
     (absFormals
      (par (idt "k2"))
      (blkCommandDec
       (bind (idt "i") (ref 0))
       (seq
        (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
        (seq
         (loop
          (gt (idt "resource") 0)
          (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
         (ret 0)))))
     "i"
     (loc 3)
     "k1"
     (loc 4)
     "k2"
     (loc 2)
     "produtor"
     (absFormals
      (par (idt "k1"))
      (blkCommandDec
       (bind (idt "co") (ref 5))
       (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
     "resource"
     (loc 1))
    (list (loc 4))
    (hash
     "consumidor"
     (absFormals
      (par (idt "k2"))
      (blkCommandDec
       (bind (idt "i") (ref 0))
       (seq
        (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
        (seq
         (loop
          (gt (idt "resource") 0)
          (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
         (ret 0)))))
     "i"
     (loc 3)
     "k2"
     (loc 2)
     "produtor"
     (absFormals
      (par (idt "k1"))
      (blkCommandDec
       (bind (idt "co") (ref 5))
       (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
     "resource"
     (loc 1))
    (list (loc 3))
    (cont
     (hash
      "consumidor"
      (absFormals
       (par (idt "k2"))
       (blkCommandDec
        (bind (idt "i") (ref 0))
        (seq
         (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
         (seq
          (loop
           (gt (idt "resource") 0)
           (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
          (ret 0)))))
      "i"
      (loc 3)
      "k2"
      (loc 2)
      "produtor"
      (absFormals
       (par (idt "k1"))
       (blkCommandDec
        (bind (idt "co") (ref 5))
        (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
      "resource"
      (loc 1))
     (list
      (hash
       "consumidor"
       (absFormals
        (par (idt "k2"))
        (blkCommandDec
         (bind (idt "i") (ref 0))
         (seq
          (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
          (seq
           (loop
            (gt (idt "resource") 0)
            (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
           (ret 0)))))
       "k2"
       (loc 2)
       "produtor"
       (absFormals
        (par (idt "k1"))
        (blkCommandDec
         (bind (idt "co") (ref 5))
         (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
       "resource"
       (loc 1))
      (list (loc 2))
      (hash
       "consumidor"
       (absFormals
        (par (idt "k2"))
        (blkCommandDec
         (bind (idt "i") (ref 0))
         (seq
          (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
          (seq
           (loop
            (gt (idt "resource") 0)
            (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
           (ret 0)))))
       "produtor"
       (absFormals
        (par (idt "k1"))
        (blkCommandDec
         (bind (idt "co") (ref 5))
         (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
       "resource"
       (loc 1))
      (list (loc 1))
      (cont
       (hash
        "consumidor"
        (absFormals
         (par (idt "k2"))
         (blkCommandDec
          (bind (idt "i") (ref 0))
          (seq
           (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
           (seq
            (loop
             (gt (idt "resource") 0)
             (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
            (ret 0)))))
        "produtor"
        (absFormals
         (par (idt "k1"))
         (blkCommandDec
          (bind (idt "co") (ref 5))
          (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
        "resource"
        (loc 1))
       '(#hash() ())
       '(print blk))
      '#hash()
      '())
     (list
      'assign
      (idt "k2")
      (seq
       (loop
        (gt (idt "resource") 0)
        (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
       (ret 0))
      'blk
      'blk
      'print
      'blk))
    (hash
     "consumidor"
     (absFormals
      (par (idt "k2"))
      (blkCommandDec
       (bind (idt "i") (ref 0))
       (seq
        (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
        (seq
         (loop
          (gt (idt "resource") 0)
          (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
         (ret 0)))))
     "k2"
     (loc 2)
     "produtor"
     (absFormals
      (par (idt "k1"))
      (blkCommandDec
       (bind (idt "co") (ref 5))
       (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
     "resource"
     (loc 1))
    (list (loc 2))
    (hash
     "consumidor"
     (absFormals
      (par (idt "k2"))
      (blkCommandDec
       (bind (idt "i") (ref 0))
       (seq
        (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
        (seq
         (loop
          (gt (idt "resource") 0)
          (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
         (ret 0)))))
     "produtor"
     (absFormals
      (par (idt "k1"))
      (blkCommandDec
       (bind (idt "co") (ref 5))
       (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
     "resource"
     (loc 1))
    (list (loc 1))
    (cont
     (hash
      "consumidor"
      (absFormals
       (par (idt "k2"))
       (blkCommandDec
        (bind (idt "i") (ref 0))
        (seq
         (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
         (seq
          (loop
           (gt (idt "resource") 0)
           (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
          (ret 0)))))
      "produtor"
      (absFormals
       (par (idt "k1"))
       (blkCommandDec
        (bind (idt "co") (ref 5))
        (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
      "resource"
      (loc 1))
     '(#hash() ())
     '(print blk))
    '#hash()
    '())
   (list
    'assign
    (idt "k1")
    (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1"))))))
    (seq (assign (idt "resource") 0) (ret 0))
    'blk
    'blk
    'assign
    (idt "k2")
    (seq
     (loop
      (gt (idt "resource") 0)
      (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
     (ret 0))
    'blk
    'blk
    'print
    'blk))
  (loc 5)
  0
  (loc 3)
  0
  (loc 4)
  (cont
   (hash
    "consumidor"
    (absFormals
     (par (idt "k2"))
     (blkCommandDec
      (bind (idt "i") (ref 0))
      (seq
       (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
       (seq
        (loop
         (gt (idt "resource") 0)
         (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
        (ret 0)))))
    "i"
    (loc 3)
    "k2"
    (loc 2)
    "produtor"
    (absFormals
     (par (idt "k1"))
     (blkCommandDec
      (bind (idt "co") (ref 5))
      (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
    "resource"
    (loc 1))
   (list
    (hash
     "consumidor"
     (absFormals
      (par (idt "k2"))
      (blkCommandDec
       (bind (idt "i") (ref 0))
       (seq
        (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
        (seq
         (loop
          (gt (idt "resource") 0)
          (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
         (ret 0)))))
     "k2"
     (loc 2)
     "produtor"
     (absFormals
      (par (idt "k1"))
      (blkCommandDec
       (bind (idt "co") (ref 5))
       (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
     "resource"
     (loc 1))
    (list (loc 2))
    (hash
     "consumidor"
     (absFormals
      (par (idt "k2"))
      (blkCommandDec
       (bind (idt "i") (ref 0))
       (seq
        (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
        (seq
         (loop
          (gt (idt "resource") 0)
          (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
         (ret 0)))))
     "produtor"
     (absFormals
      (par (idt "k1"))
      (blkCommandDec
       (bind (idt "co") (ref 5))
       (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
     "resource"
     (loc 1))
    (list (loc 1))
    (cont
     (hash
      "consumidor"
      (absFormals
       (par (idt "k2"))
       (blkCommandDec
        (bind (idt "i") (ref 0))
        (seq
         (if-my-struct (lt (idt "resource") 1) (assign (idt "k2") (call/cc (idt "k2"))) (nop))
         (seq
          (loop
           (gt (idt "resource") 0)
           (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
          (ret 0)))))
      "produtor"
      (absFormals
       (par (idt "k1"))
       (blkCommandDec
        (bind (idt "co") (ref 5))
        (seq (loop (gt (idt "co") 0) (seq (assign (idt "co") (sub (idt "co") 1)) (seq (assign (idt "resource") 2) (assign (idt "k1") (call/cc (idt "k1")))))) (seq (assign (idt "resource") 0) (ret 0)))))
      "resource"
      (loc 1))
     '(#hash() ())
     '(print blk))
    '#hash()
    '())
   (list
    'assign
    (idt "k2")
    (loop
     (gt (idt "resource") 0)
     (seq (print (idt "resource")) (seq (assign (idt "resource") (sub (idt "resource") 1)) (if-my-struct (eq (idt "resource") 0) (assign (idt "k2") (call/cc (idt "k2"))) (nop)))))
    (ret 0)
    'blk
    'blk
    'print
    'blk))
  (loc 1)
  0)
 '()
 '()
 '(0 1 2 1 2 1 2 1 2 1 2)))




   )
