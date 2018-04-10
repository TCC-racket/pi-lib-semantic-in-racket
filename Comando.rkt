#lang racket

(struct comando (U seq init atrib print exit))
(struct seq (comando1 comando2)
