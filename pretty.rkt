#lang racket

(provide my-struct)
(define my-pi-equivalence (hash
			    "piAutomata" "δ"))

(define pick-fields (hash))

(define (filter-fields struct-name full-attr)
  ((hash-ref pick-fields struct-name (lambda() (lambda (x) x))) full-attr))

(define (my-pretty-print-name str)
  (hash-ref my-pi-equivalence str str))

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

(define (write-loc node port mode)
  (let* ((write-fun (if mode write display)))
    (if (struct? node)
      (let* ((struct-decompose (struct->list node))
	     (struct-full-name (symbol->string (car struct-decompose)))
	     (struct-name (string-replace struct-full-name "struct:" ""))
	     (pretty-print-name (pretty-print-struct-name struct-name))
	     (atributes (cdr struct-decompose)))
	      (begin
		    (write-fun pretty-print-name port)
		    (write-fun "(" port)
		    (newline)
		    (if (empty? atributes)
		      (void)
		      (begin
			(write-loc (car atributes) port mode)
			(for ((element (cdr atributes)))
			     (write-loc element port mode ))))
		    (write-fun ")" port)))
      (if (list? node)
          (if (null? node)
              (write-loc "[]" port mode)
              (begin
		(write-loc "[" port mode)
                (write-loc (car node) port mode)
                (for ((element (cdr node)))
                  (write-loc element port mode))
		(write-loc "]" port mode)))
	     (begin
		      (write-fun node port))))))

(define-syntax-rule (my-struct name args . rest)
	(struct name args #:transparent #:methods gen:custom-write [(define write-proc write-loc)]))

(module+ test
  ;write test creating string-ports
  ;we can use display with a port
	 (my-struct loc (a))
	 (display (loc 12))
	 (display (loc (loc 12)))
	 (display (loc (loc (loc 12)))))
