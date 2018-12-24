#lang peg

_ < [ \t\n]* ;
EOI < ! . ;
top <- _ v:file _ EOI -> v;
file <- v1:module _ v2:command -> (blk v1 v2);
module <- 'module' _ identifier _ v1:globalVariable _ v2:proc _ v3:fun _ v4:gen-> (dec v1 (dec v2 (dec v3 v4)));
identifier <- v:[a-zA-Z]+ -> (idt v);
globalVariable <- 
