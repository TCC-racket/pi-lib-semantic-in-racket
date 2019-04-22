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
end;
           x=call/cc(f);
           if(x<5)
           {
                print(x);
                k(x+1);
           };
           ")
  );

_ < [ \t\n]* ;
EOI < ! . ;
top <- _ v:file _ EOI -> v;
file <- v1:module _ v2:command -> (blkComandDec v1 v2);
module <- 'module' _ identifier _ v1:globalVariable _ v2:proc _ v3:fun _ v4:gen _ 'end;' -> (dec v1 (dec v2 (dec v3 v4)));
identifier <- v:[a-zA-Z/]+ -> (idt v);
globalVariable <- v1:var _ v2:const _ v3:init -> (combine v1 v2 v3);
command <- s:(simple-command _ (~';' _ simple-command _)*) ';'? -> (foldr seq (nop) s);
simple-command <- assign / return / conditionals / print / proc-call;
proc-call <- proc-call-with-parameters;
proc-call-with-parameters <- v:identifier _ '(' _ a:(expression (~',' _ expression _)*) _ ')' -> (prc-formals v ;
print <- 'print' _ '(' _ e:expression _ ')' -> (print-struct e);
conditionals <- 'if' _ '(' _ v:bool-expression  _')' _ b:block -> (if-struct v b (nop));
return <- 'return' _ e:expression -> (ret e);
assign <- i:identifier _ '=' _ e:expression -> (assign i e);
nop <- 'nop;' -> (nop);
gen <- 'gen' / '';
fun <- fun-with-formals / fun-without-formals / no-fun ;
fun-with-formals <- 'fun' _ v:identifier _ '(' _ l:formals _ ')' _ b:block -> (funFormals v l b);
fun-without-formals <- 'fun' _ v:identifier _ '(' _ ')' _ b:block -> (fun v b);
no-fun <- '' -> (noDec);
formals <- l:(identifier _ (~',' _ identifier _)*) -> (let ((r (map par l))) (foldr for (car r) (cdr r)));
block <- '{' _ d:dec _ c:command _ '}' -> (blkComandDec d c);
dec <- '' -> (noDec);
proc <- 'proc' / '';
init <- 'init' _ l:(single-init (~',' _ single-init _)*) _ ~(';') / l:'' -> (if (list? l) l '());
single-init <- identifier _ ~'=' _ expression;
const <- 'const' _ l:(identifier _ (~',' _ identifier _)*) _ ~(';') / l:'' -> (if (list? l) l '());
var <- 'var' _ l:(identifier _ (~',' _ identifier _)*) _ ~(';') / l:'' -> (if (list? l) l '());

expression <- bool-expression / arith-expression;
bool-expression <- relational-operations / bool-literals ;
relational-operations <- lesser-then;
lesser-then <- a:arith-expression _ '<' _ b:arith-expression -> (lt a b);
arith-expression <- sum ;
sum <- v:(prod _ (~'+' _ prod _)*) -> (foldr add (car v) (cdr v));
prod <- number / call-cc / function-call / identifier;
function-call <- function-call-with-parameters ;
function-call-with-parameters <- v:identifier _ '(' _ a:(expression (~',' _ expression _)*) _ ')' -> (calAtuals v (foldr act (car a) (cdr a)));
call-cc <- 'call/cc' _ '(' _ i:identifier _ ')' -> (call/cc-struct i);
bool-literals <- v:('true' / 'false') -> (equal? v "true");
number <- v:([0-9]+) -> (string->number v);








