#lang racket

(require peg/peg)
(require "programa.rkt")
(require "Comando.rkt")
(require "smc.rkt")

(define teste1 (lambda() (executeSMC
                          (comandoConv
                           (peg bloco
"
 var y = 6, x = 1;
 x := y+5;
 if(x == 42){
  print(y)
 } else { var x = 3;
  print(x);
  print(y)}")))))

(define teste2 (lambda() (executeSMC
                          (comandoConv
                           (peg bloco
"
var y = 1, x = 19000;
  while ~ (x == 0)
    do { y := y * x ;
         x := x - 1}; 
print(y)")))))
