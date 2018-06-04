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
;(define smc (executeSMC (comandoConv (peg comando (port->string in)))))


(module+ test
	(require peg/peg)
	(require rackunit)


	(test-suite "if com chaves, com declarações, sem comandos"

		(define saidaParser (peg comando "
			if (true)
			{
				var x := 0, y:= 8, z := 12;
			}"))
		
		(check-equal? 
			saidaParser
			()) ;;Raphael, o que o parser deveria retornar aqui??
		(define saidaBPLC (comandoConv saidaParser))
		
		(check-equal?
			saidaBPLC
			()) ;;Luis, o que a tradução pro bplc deveria ser??
		(define saidaSMC (executeSMC saidaBPLC))

		(check-equal?
			saidaSMC
			())) ;;João, o que o smc deveria devolver??
	(test-suite "if com chaves, sem declarações, com comandos"

		(define saidaParser (peg comando "
			if (true)
			{
				print(1);
			}"))
		
		(check-equal? 
			saidaParser
			()) ;;Raphael, o que o parser deveria retornar aqui??
		(define saidaBPLC (comandoConv saidaParser))
		
		(check-equal?
			saidaBPLC
			()) ;;Luis, o que a tradução pro bplc deveria ser??
		(define saidaSMC (executeSMC saidaBPLC))

		(check-equal?
			saidaSMC
			())) ;;João, o que o smc deveria devolver??

	(test-suite "if com chaves, com multiplas declarações, sem comandos"
		(define saidaParser (peg comando "
			if (true)
			{
				var x := 0, y:= 8, z := 12;
				const a:= x, x := 23;
			}
		"))
		(check-equal? 
			saidaParser
			()) ;;Raphael, o que o parser deveria retornar aqui??
		(define saidaBPLC (comandoConv saidaParser))
		
		(check-equal?
			saidaBPLC
			()) ;;Luis, o que a tradução pro bplc deveria ser??
		(define saidaSMC (executeSMC saidaBPLC))

		(check-equal?
			saidaSMC
			())) ;;João, o que o smc deveria devolver??
	(test-suite "if com chaves, com uma declaração, com if interno, com chaves, com declaração e comando, seguido por comando"
		(define saidaParser (peg comando "
			if (true)
			{
				var x := 0;
				if(true)
				{
					var x := 1;
					print(x);
				}
				print(x);
			}"))
		(check-equal? 
			saidaParser
			()) ;;Raphael, o que o parser deveria retornar aqui??
		(define saidaBPLC (comandoConv saidaParser))
		
		(check-equal?
			saidaBPLC
			()) ;;Luis, o que a tradução pro bplc deveria ser??
		(define saidaSMC (executeSMC saidaBPLC))

		(check-equal?
			saidaSMC
			())) ;;João, o que o smc deveria devolver??


	(test-suite "if com chaves, com uma declaração,com um if interno, sem chaves com declaração,seguido por  comando e outro comando"
		(define saidaParser (peg comando "
			if (true)
			{
				var x := 0;
				if(true)
					var x:=1;
					print(x);
				print(x);
			}"))
		(check-equal? 
			saidaParser
			()) ;;Raphael, o que o parser deveria retornar aqui??
		(define saidaBPLC (comandoConv saidaParser))
		
		(check-equal?
			saidaBPLC
			()) ;;Luis, o que a tradução pro bplc deveria ser??
		(define saidaSMC (executeSMC saidaBPLC))

		(check-equal?
			saidaSMC
			())) ;;João, o que o smc deveria devolver??

