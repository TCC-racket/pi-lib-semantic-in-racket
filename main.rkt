#lang racket

(require peg/peg)
(require "programa.rkt")
(require "Comando.rkt")
(require "smc.rkt")

;(define in (open-input-file "C:\\Users\\Pichau\\Desktop\\8 PERÍODO\\COMPILADORES\\RACKET TEST\\fact.imp"))

;(define l1(lambda()(read-string 1000 in)))

;(define pgrm(lambda()(peg programa (read-string 1000 in))))

;(l1) ;o que tem dentro do arquivo

;(pgrm) ;peg programa "l1"


;(define in (open-input-file (vector-ref (current-command-line-arguments) 0)))
;(peg programa (port->string in))

(module+ test
  (require peg/peg)
  (executeSMC
    (comandoConv (peg comando "x := 5 ;
    y := 1 ;
		while ~ (x == 0)
        do {               
		  y := y * x ;
          x := x - 1
		} ; 
		print(y)
	}"))))
