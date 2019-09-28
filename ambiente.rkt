#lang racket

(require racket/struct)
(require "pretty.rkt")
(provide clean identifier atrib reference constant listLoc? loc loc?)

(define (write-loc location port mode)
  (let ((write-fun (if mode write display)))
    (write-fun "Loc(" port)
    (write-fun (loc-adress location) port)
    (write-fun ")" port)))

(struct loc (adress) #:methods gen:custom-write [(define write-proc write-loc)])

; (-> any bool)
(define (listLoc? l)
	(and (list? l) (andmap loc? l)))

; (-> listLoc? (hash loc? (U bool number)) (hash loc? (U bool number)))
(define (clean locali mem)
	(foldl (compose hash-remove (lambda (x y) (values y x))) mem locali ))	

; (-> (hash string (U loc bool number)) (hash loc (U bool number) string (U bool number)))
(define (identifier envi mem id)
	(if (not (hash-has-key? envi id)) (raise "variavel não declarada\n")
		(if (not (loc? (hash-ref envi id))) (hash-ref envi id)
			(hash-ref mem (hash-ref envi id)))))

; (-> hash mem string (U bool number) mem)
(define (atrib envi memory id value)
  (if (loc? (hash-ref envi id))
	(hash-set memory (hash-ref envi id) value) (raise "não pode atribuir para constantes\n")))


; (-> env (hash loc (U bool number)) string (U bool number))
(define (reference env memory ident value)
	(let ([newLoc (add1 (hash-count memory))]) 
		(values (hash-set memory (loc newLoc) value) (hash-set env ident (loc newLoc)))))


; (-> env string (U bool number))
(define (constant env i value)
	(hash-set env i value))


