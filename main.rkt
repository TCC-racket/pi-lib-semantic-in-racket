#lang racket

(require peg/peg)
(require "programa.rkt")
(require "Comando.rkt")
(require "smc.rkt")

(define teste1 (lambda() (executeSMC
                          (comandoConv
                           (peg programa
"
 module Fact-Rec
   var y
   init y=1
   proc fact(x) { 
      if ~(x == 0)   
      {
		y := y*x ;
		fact(x-1) 
      }
      else print(y) 
   }
end")(display 10)))))


;chamada dos testes

(teste1)
