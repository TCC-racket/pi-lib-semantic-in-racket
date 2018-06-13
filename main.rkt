#lang racket

(require peg/peg)
(require "programa.rkt")
(require "Comando.rkt")
(require "smc.rkt")

(define teste1 (lambda() (executeSMC
                          (comandoConv
                           (peg comando
"if(true){
 var y = 6, x = 1;
 x := y+5;
 if(x == 11){
  print(y)
 }
  print(x);
  print(y)
}")))))

(define teste2 (lambda() (executeSMC
                          (comandoConv
                           (peg comando
"if(true){
var y = 1, x = 100;
  while ~ (x == 0)
    do { y := y * x ;
         x := x - 1}; 
print(y)
}")))))



;call tests

(teste1)
(teste2)
