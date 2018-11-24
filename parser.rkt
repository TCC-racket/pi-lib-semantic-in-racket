#lang peg

_ < [ \t\n]* ;
EOI < ! . ;
programa <- _ v1:modulo _ v2:comando _ EOI -> (blk v1 v2);
modulo <- 'module' _ identifier _ v1:globalVariable _ v2:proc _ v3:fun -> (dec v1 (dec v2 v3));
identifier <- v:[a-zA-Z]+ -> (idt v);
globalVariable <- 
