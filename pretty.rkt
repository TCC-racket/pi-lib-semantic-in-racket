#lang racket

(provide my-struct)
(define my-pi-equivalence (hash
			    "piAutomata" "δ"))

(define (struct->list s)
  (vector->list (struct->vector s)))

(define (capitalize-first-letter s)
  (let* ((s-in-list (string->list s))
	 (first-element (car s-in-list))
	 (rest-of-s (cdr s-in-list))
	 (cap-first-element (char-upcase first-element)))
    (list->string (cons cap-first-element rest-of-s))))


(define (pretty-print-struct-name s)
  (hash-ref my-pi-equivalence s (capitalize-first-letter s)))

(define (on-depth str depth)
  (string-append
	  (let loop ((depth depth)
		     (acc ""))
	    (if (equal? depth 0)
	      acc
	      (loop (- depth 1) (string-append acc " "))))
	  (format "~a" str)))

(define (write-loc node port mode [depth 0])
  (let* ((write-fun (if mode write display)))
    (if (struct? node)
      (let* ((struct-decompose (struct->list node))
	     (struct-full-name (symbol->string (car struct-decompose)))
	     (struct-name (string-replace struct-full-name "struct:" ""))
	     (pretty-print-name (pretty-print-struct-name struct-name))
	     (atributes (cdr struct-decompose)))
	      (begin
		    (write-fun (on-depth pretty-print-name depth) port)
		    (write-fun (on-depth "(" depth) port)
		    (newline)
		    (if (empty? atributes)
		      (void)
		      (begin
			(write-loc (car atributes) port mode (+ 1 depth))
			(for ((element (cdr atributes)))
			     (write-loc element port mode (+ 1 depth)))))
		    (write-fun (on-depth ")" depth) port)))
      (if (list? node)
          (if (null? node)
              (write-loc "[]" port mode depth)
              (begin
		(write-loc "[" port mode depth)
                (write-loc (car node) port mode (+ 1 depth))
                (for ((element (cdr node)))
                  (write-loc element port mode (+ 1 depth)))
		(write-loc "]" port mode depth)))
	     (begin
		      (write-fun (on-depth node depth) port))))))

(define-syntax-rule (my-struct name args . rest)
	(struct name args #:transparent #:methods gen:custom-write [(define write-proc write-loc)]))


(module+ test
	 (my-struct loc (a))
	 (display (loc 12))
	 (display (loc (loc 12)))
	 (display (loc (loc (loc 12)))))
