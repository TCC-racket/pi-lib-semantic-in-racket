(struct smc (val amb control))
;(require "Stack.rkt")
;(require "Contexto.rkt")
(require "AritExp.rkt")
(provide executeSMC)
(define (executeSMC bplc)
  (car (smc-val (smcEval (smc '() (hash) (list bplc))))))
  
(define (smcEval smcP)
  (match smcP [(smc d e (list (add a b) c ...)) (smcEval (smc d e (append (list a b 'add) c)))]
              [(smc d e (list (sub a b) c ...)) (smcEval (smc d e (append (list a b 'sub) c)))]
              [(smc d e (list (mult a b) c ...)) (smcEval (smc d e (append (list a b 'mult) c)))]
              [(smc d e (list (div a b) c ...)) (smcEval (smc d e (append (list a b 'div) c)))]
              
              [(smc d e (list (? number? a) b ...)) (smcEval (smc (cons a d) e b))]
              [(smc (list (? number? a) (? number? b) c ...) d (list 'add e ...)) (smcEval (smc (cons (+ a b) c) d e))]
              [(smc (list (? number? a) (? number? b) c ...) d (list 'sub e ...)) (smcEval (smc (cons (- a b) c) d e))]
              [(smc (list (? number? a) (? number? b) c ...) d (list 'mult e ...)) (smcEval (smc (cons (* a b) c) d e))]
              [(smc (list (? number? a) (? number? b) c ...) d (list 'div e ...)) (smcEval (smc (cons (/ a b) c) d e))]
              
              [a a]))
