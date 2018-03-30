#lang racket

(provide (all-from-out))

(define-peg space #\space)
(define-peg tab (or #\t #\v))
(define-peg newLine 
                      (or
                          #\n ; para sistemas de verdade
                          (and #\n #\f))) ; para o windows
                          
(define-peg separadoresDePalavras (* (or space tab newLine)))
