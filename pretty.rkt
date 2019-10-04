#lang racket

(provide my-struct)
(define my-pi-equivalence (hash
			    "loc" "Loc"
			    "num" "Num"
			    "piAutomata" ""))

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
          (if (null? node)
              (write-loc "[]" port mode)
              (begin
                (write-loc (car node) port mode)
                (for ((element (cdr node)))
                  (write-fun " :: " port)
                  (write-loc element port mode))))
	     (begin
		      (write-fun node port))))))

(define-syntax-rule (my-struct name args . rest)
	(struct name args #:transparent #:methods gen:custom-write [(define write-proc write-loc)]))

