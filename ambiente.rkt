#lang racket

(provide env env? envClean envIdt envAssign newEnv)

(struct loc (a) #:transparent)


(struct env (realEnv positionFree) #:transparent)

(define newEnv (env (hash) 0))

(define (envClean envi mem)
	mem)

(define (envIdt envi mem id)
	(if (not (hash-has-key? envi id)) (raise "variavel não declarada\n")
		(if (not (loc? (hash-ref envi id))) (hash-ref envi id)
			(hash-ref mem (hash-ref envi id)))))


(define (envAssign envi memory id value)
	(hash-set memory (hash-ref envi id) value))
	



