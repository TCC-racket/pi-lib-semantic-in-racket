#lang racket

(require peg/peg)
(provide peg-rule:space)
(provide peg-rule:tab)
(provide peg-rule:newLine)
(provide peg-rule:spaces)
(provide peg-rule:tabs)
(provide peg-rule:newLines)
(provide peg-rule:wordSeparatorWithoutNewLines)
(provide peg-rule:wordSeparator)


(define-peg space #\space)
(define-peg tab (or "\t" "\v"))
(define-peg newLine 
                      (or
                          "\n" ; para sistemas de verdade
                          (and "\n" "\f"))) ; para o windows
;(define-peg newLine #\newline)
(define-peg spaces (* space))
(define-peg tabs (* tab))
(define-peg newLines (* newLine))

(define-peg wordSeparatorWithoutNewLines (* (or space tab)))
(define-peg wordSeparator (* (or space tab newLine)))
