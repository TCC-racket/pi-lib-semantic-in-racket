#lang racket

(provide clean identifier atrib reference constant)

(struct loc (adress) #:transparent)

(define (clean locali mem)
	(foldl (compose hash-remove (lambda (x y) (values y x))) mem locali ))	

(define (identifier envi mem id)
	(if (not (hash-has-key? envi id)) (raise "variavel não declarada\n")
		(if (not (loc? (hash-ref envi id))) (hash-ref envi id)
			(hash-ref mem (hash-ref envi id)))))


(define (atrib envi memory id value)
	(hash-set memory (hash-ref envi id) value))
	
(define (reference env memory ident value)
	(let ([newLoc (add1 (hash-count memory))]) 
		(values (hash-set memory (loc newLoc) value) (hash-set env ident (loc newLoc)))))



(define (constant env i value)
	(hash-set env i value))


