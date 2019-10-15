#lang racket

(provide my-struct)
(define my-pi-equivalence (hash
			    "piAutomata" "δ"))

(define pick-fields (hash "piAutomata" (lambda (l) (match l
                                                     [(list env val mem control loc output)
                                                      (list control val env mem loc)]))))

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
	     (atributes (filter-fields struct-name (cdr struct-decompose))))
	      (begin
		    (write-fun pretty-print-name port)
		    (write-fun "(" port)
		    (if (empty? atributes)
		      (void)
		      (begin
			(write-loc (car atributes) port mode)
			(for ((element (cdr atributes)))
                             (write-loc "," port mode)
			     (write-loc element port mode ))))
		    (write-fun ")" port)))
      (if (list? node)
          (if (null? node)
              (write-loc "[]" port mode)
              (begin
                (write-loc (car node) port mode)
                (write-loc "::" port mode)
                (write-loc (cdr node) port mode)))
          (if (number? node)
              (write-loc (format "Num(~a)" node) port mode)
              (if (hash? node)
                  (begin
                    (write-loc "{" port mode)
                    (for ((k (in-hash-keys node)))
                      (write-loc k port mode)
                      (write-loc "->" port mode)
                      (write-loc (hash-ref node k) port mode)
                      (write-loc " " port mode))
                    (write-loc "}" port mode))
                  (if (symbol? node)
                      (begin
                        (write-loc "#" port mode)
                        (write-loc (symbol->string node) port mode))
                      (begin
                        (write-fun node port)))))))))

(define-syntax-rule (my-struct name args . rest)
	(struct name args #:transparent #:methods gen:custom-write [(define write-proc write-loc)]))

(module+ test
  (require rackunit)

  (my-struct loc (a))
  (my-struct dub (a b))
  (my-struct piAutomata (env val mem control loc output))
  (my-struct idt (a))
  (define (test-function structure expected)
    (define strPort (open-output-string))
    (display structure strPort)
    (check-equal? expected (get-output-string strPort)))

  (test-function (loc 12) "Loc(Num(12))")
  (test-function (loc (list 1 2 3)) "Loc(Num(1)::Num(2)::Num(3)::[])")
  (test-function (dub 1 2) "Dub(Num(1),Num(2))")
  (test-function
   (piAutomata (hash (idt "x") (loc 2)) (list) (hash (loc 2) 2) (list) (list) (list))
   "δ([],[],{Idt(x)->Loc(Num(2)) },{Loc(Num(2))->Num(2) },[])")
  (test-function (loc 'hi) "Loc(#hi)"))
