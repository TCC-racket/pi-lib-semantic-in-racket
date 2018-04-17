#lang racket
(struct Contextos (a b)) ; a é o contexto global e b é a hierarquia de contextos
                          ; essa escolha teve basicamente, motivações performaticas
                          ; embora para mim, João Pedro, pareça bem elegante também
(define novaListaDeContextos (Contextos (hash) '()))
(define (addContexto contextos) (match contextos [(Contextos a b) (Contextos a (cons (hash) b)) ]   )
(define (destroyContexto contextos) (match contextos [(Contextos a b) (Contextos a (cdr b) )] )
(define (addVariavelContexto contextos variavel)
(define (variavelNoContexto? contextos variavel)
(define (modificarVariavel contextos variavel valor)
