#lang peg

(require "structures.rkt");
(provide parser);

(define (parser str)
  (peg top str));
(define (combine a b c)
  (match (list a b c)
    [(list (list (idt a) b ...) l2 (list (idt a) e r ...)) (dec (ref (idt a) e) (combine b l2 r))]
    [(list l2 (list (idt a) b ...) (list (idt a) e r ...)) (dec (cns (idt a) e) (combine b l2 r))]
    [(list (list) (list) (list)) (noDec)]));

(module+ test
  (require rackunit)
  (parser "module ola
           var x,k;
           init x=0,k=false;
           fun f(cont)
           {
                k=cont;
                return 0;
           }
           x=call/cc(f);
           if(x<5)
           {
                print(x);
                k(x+1);
           }
           end;
           ")
  );

_ < [ \t\n]* ;
EOI < ! . ;
top <- _ v:file _ EOI -> v;
file <- v1:module _ v2:command -> (blkComandDec v1 v2);
module <- 'module' _ identifier _ v1:globalVariable _ v2:proc _ v3:fun _ v4:gen _ 'end;' -> (dec v1 (dec v2 (dec v3 v4)));
identifier <- v:[a-zA-Z]+ -> (idt v);
globalVariable <- v1:var _ v2:const _ v3:init -> (combine v1 v2 v3);
command <- nop;
nop <- 'nop;';
gen <- 'gen' / '';
fun <- 'fun' / '';
proc <- 'proc' / '';
init <- 'init' _ l:(single-init (~',' _ single-init _)*) _ ~(';') / l:'' -> (if (list? l) l '());
single-init <- identifier _ ~'=' _ expression;
const <- 'const' _ l:(identifier _ (~',' _ identifier _)*) _ ~(';') / l:'' -> (if (list? l) l '());
var <- 'var' _ l:(identifier _ (~',' _ identifier _)*) _ ~(';') / l:'' -> (if (list? l) l '());

expression <- bool-expression / arith-expression;
bool-expression <- bool-literals ;
arith-expression <- number;
bool-literals <- v:('true' / 'false') -> (equal? v "true");
number <- v:([0-9]+) -> (string->number v);