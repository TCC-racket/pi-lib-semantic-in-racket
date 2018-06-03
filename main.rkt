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

(executeSMC (comandoConv (peg comando "
if (true)
{
	var x := 0, y:= 8, z := 12;
}
"))) ; deve retornar um ambiente vazio, uma memoria vazia e tudo mais vazio

(executeSMC (comandoConv (peg comando "
if (true)
{
	print(1);
}
"))) ; deve retornar a mesma coisa do anterior

(executeSMC (comandoConv (peg comando "
if (true)
{
	var x := 0, y:= 8, z := 12;
	const a:= x, x := 23;
}
"))) ; deve retornar um erro pois x esta sendo declarado duas vezes

(executeSMC (comandoConv (peg comando "
if (true)
{
	var x := 0;
	if(true)
	{
		var x := 1;
		print(x);
	}
	print(x);
}
"))) ; deve printar 1 e depois 0

(executeSMC (comandoConv (peg comando "
if (true)
{
	var x := 0;
	if(true)
		var x:=1;
		print(x);
	print(x);
}
"))) ; deve printar 0 duas vezes, pois sem chaves, o primeiro print esta fora do if mais interno





)
