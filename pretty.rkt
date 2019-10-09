#lang racket

(provide my-struct)
(define my-pi-equivalence (hash
			    "piAutomata" "PI"))

(define pick-fields (hash))

(define (filter-fields struct-name full-attr)
  ((hash-ref pick-fields struct-name (lambda() (lambda (x) x))) full-attr))

(define (my-pretty-print-name str)
  (hash-ref my-pi-equivalence str str))

(define (struct->list s)
  (vector->list (struct->vector s)))

(define (make-indent f p i)
  (when (not (equal? i 0))
    (f " " p)
    (make-indent f p (sub1 i))))

(define (write-loc node port mode [indent 0])
  (let* ((write-fun (if mode write display)))
    (if (struct? node)
      (let* ((struct-decompose (struct->list node))
	     (struct-full-name (symbol->string (car struct-decompose)))
	     (struct-name (string-replace struct-full-name "struct:" ""))
	     (pretty-print-name (my-pretty-print-name struct-name))
	     (atributes (filter-fields struct-name (cdr struct-decompose))))
	      (begin
                    (make-indent write-fun port indent)
		    (write-fun pretty-print-name port)
                    (newline)
		    (if (empty? atributes)
		      (void)
		      (begin
			(write-loc (car atributes) port mode (+ 2 indent))
			(for ((element (cdr atributes)))
                             (newline)
			     (write-loc element port mode (+ 2 indent)))))
                    (newline)))
      (if (list? node)
          (if (null? node)
              (write-loc "[]" port mode indent)
              (begin
                (write-loc (car node) port mode indent)
                (for ((element (cdr node)))
                  (write-fun " :: " port)
                  (write-loc element port mode))))
	     (begin
                      (make-indent write-fun port indent)
		      (write-fun node port))))))

(define-syntax-rule (my-struct name args . rest)
	(struct name args #:transparent #:methods gen:custom-write [(define write-proc write-loc)]))

(my-struct loc (a))

(my-struct piAutomata (a b))

(display (piAutomata (loc (list 1 3)) (loc 2)))
