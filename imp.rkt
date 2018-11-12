#lang peg

//pi-automata
(struct pi-automata (E S M C O) #:transparent);

//pi-lib
(struct add (a b) #:transparent);
(struct sub (a b) #:transparent);
(struct mult (a b) #:transparent);
(struct div-struct (a b) #:transparent);
(struct callf (name actuals) #:transparent);
(struct identifier (name) #:transparent);
(struct act (a b) #:transparent);


(define (is-left-associative v l)
  (if (null? l)
      v
      (is-left-associative ((car l) v (cadr l)) (cddr l))));


(define (pi-automata-eval pi)
  (if (and
       (null? (pi-automata-C pi)))
      pi
      (pi-automata-eval
       (match pi
         [(pi-automata E S M (list (? number? a) b ...) O) (pi-automata E (cons a S) M b O)]))));

(define (process-pi-lib pi-lib)
  (pi-automata-eval (pi-automata (hash) '() (hash) (list pi-lib) '())));







//parser


_ < [ \t\n]*;

exp <-  arit-exp; //implement call/cc

arit-exp <- _ v:sum _ -> v;
sum <- v1:multiply _ v2:((sum-operator _ multiply _)*) -> (is-left-associative v1 v2);
sum-operator <- v:('+' / '-') -> (if (equal? v "+") add sub);

multiply <- v1:base _ v2:((multiply-operator _ base _)*) -> (is-left-associative v1 v2);
multiply-operator <- v:('*' / '/') -> (if (equal? v "*") mult div-struct);

base <- number / arith-exp-between-parentesis / function-call / variable;

arith-exp-between-parentesis <- '(' v:arit-exp ')' -> v;
number <- v:[0-9]+ -> (string->number v);

function-call <- n:identifier _ '(' _ l:list-arguments _ ')' -> (callf n l);
list-arguments <- actual _ (separator-actuals _ actual _)* / '';
actual <- exp;
separator-actuals <- ',' -> act;
identifier <- v:([a-zA-Z]+) -> (identifier v);

variable <- identifier;
