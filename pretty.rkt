#lang racket

(provide register-struct write-loc)
(define my-pi-equivalence (make-hash))

(define (register-struct name pi-framework)
   (hash-set! my-pi-equivalence name pi-framework))


(define (struct->list s)
  (vector->list (struct->vector s)))


(define (write-loc node port mode)
  (let* ((write-fun (if mode write display)))
    (if (struct? node)
      (let* ((struct-decompose (struct->list node))
	     (struct-full-name (symbol->string (car struct-decompose)))
	     (struct-name (string-replace struct-full-name "struct:" ""))
	     (pretty-print-name (hash-ref my-pi-equivalence struct-name))
	     (atributes (cdr struct-decompose)))
	      (begin
		    (write-fun pretty-print-name port)
		    (write-fun "(" port)
		    (if (empty? atributes)
		      (void)
		      (begin
			(write-loc (car atributes) port mode)
			(for ((element (cdr atributes)))
			     (write-fun ";" port)
			     (write-loc element port mode))))
		    (write-fun ")" port)))
      (if (list? node)
 	     (begin
		(write-loc (car node) port mode)
		(for ((element (cdr node)))
		     (write-fun " :: " port)
		     (write-loc element port mode)))
	     (begin
		      (write-fun node port))))))
(struct loc (a b) #:transparent #:methods gen:custom-write [(define write-proc write-loc)])
(register-struct "loc" "Stallman")
(display (loc (list 1 2 3 4) (loc 1 2)))
(newline)
