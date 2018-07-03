#lang racket

(require peg/peg)
(require "atribuicao.rkt")
(require (rename-in "BoolExp.rkt" [and andB] [or orB]))
(require "AritExp.rkt")
(require "Reservadas.rkt")
(require "espacos.rkt")
(require "Exp.rkt")
(provide peg-rule:seq)
(provide peg-rule:choiceOp)
(provide peg-rule:comando)
(provide peg-rule:bloco)
(provide peg-rule:cmdUnit)
(provide nop? if? if-cond if-then if-else nop peg-rule:bloco peg-rule:declaracao)
(provide seq choice)
(provide call)
(provide peg-rule:functF)
(provide peg-rule:returnR)
;(provide peg-rule:blocoF)
;(provide peg-rule:comandoF)
(provide cal)

(struct block (declarations commands) #:transparent)
(struct comando (U seq init atrib print exit) #:transparent)
(struct seq (comando1 comando2) #:transparent)
(struct prnt(a) #:transparent)
(struct choice (comando1 comando2) #:transparent)
(struct exit(a) #:transparent)
(struct nop () #:transparent)
(struct blk (decSeq seq) #:transparent)
(struct idt (nome) #:transparent)
(struct call (nome arg) #:transparent)
(struct callF (nome arg) #:transparent)
;(struct blkF (decSeq seq) #:transparent)
(struct rtn (exp) #:transparent)


;Parte de programa / função -> call
(define-peg ident (name nome (and  
                          (or (range #\a #\z) (range #\A #\Z))
                          (* (and (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9))
                              (* (or "_" "-")
                                 (or (range #\a #\z) (range #\A #\Z))
                                 (and (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))))) (idt nome) )

(define-peg expAUX (and wordSeparator
                            (name t1 (or boolExp aritExp functF))
                            (? wordSeparator virg wordSeparator
                               (name t2 expAUX)))
  (cond [t2 (cons t1 t2)] [else (list t1)]))

(define-peg expList (? (and wordSeparator
                             (name t1 (or boolExp aritExp functF))
                             (? wordSeparator virg wordSeparator (name t2 expAUX))))
  (cond [t2 (cons t1 t2)] [else  t1 ]))

(define-peg funct (and wordSeparator (name t1 ident) wordSeparator
                       "(" wordSeparator (name t2 expList)
                       wordSeparator ")" wordSeparator) (call t1 t2))

(define-peg functF (and wordSeparator (name t1 ident) wordSeparator
                       "(" wordSeparator (name t2 expList)
                       wordSeparator ")" wordSeparator) (callF t1 t2))
#|
(define-peg blocoF (and wordSeparator (name t1 declaracao)
                       wordSeparator pointvirg wordSeparator
                       (name t2 comandoF))
  (blkF t1 t2))

(define-peg comandoF (or seqF choiceOp cmdUnitF))

(define-peg seqF(and (name t1 cmdUnitF)
                     (?(name sep separador)
                       (name t2 seqF)))
  (cond [t2 (seq t1 t2)] [else t1]))

(define-peg cmdUnitF (or atribuicao condicional loop  print exit returnR funct))
|#
(define-peg returnR (and wordSeparator return spaces (name t1 (or boolExp aritExp functF variable string )) spaces) (rtn t1))

;--------------------------------------------------------------------------------------

(define-peg separador(and wordSeparator (or virg pointvirg newLines) wordSeparator))

(define-peg seq(and (name t1 cmdUnit) (?(name sep separador) (name t2 seq))) (cond [t2 (seq t1 t2)] [else t1]))

(define-peg print (and wordSeparator printR"(" spaces (name t1 (or boolExp aritExp functF variable string )) spaces ")") (prnt t1))

(define-peg exit(and wordSeparator exitR"(" spaces (name t1 (or boolExp aritExp functF)) spaces ")") (exit t1))

;Agora com funct
(define-peg cmdUnit (or atribuicao condicional loop print exit funct))

;(define-peg comando (and (name value (or seq choiceOp cmdUnit)) (drop (? (and wordSeparator pointvirg)))) value)
(define-peg comando (or seq choiceOp cmdUnit))
                         
(define-peg choiceOp (and (name t1 cmdUnit) (? (and bar (name t2 choiceOp)))) (choice t1 t2))

(define-peg statement (or declaracao comando))

;(define-peg bloco (and wordSeparator (name t1 (? declaracao)) wordSeparator (name t2 (? comando)))
;  (cond [(not (empty? t1)) (blk t1 (cond  [(not (empty? t2)) t2] [else (nop)]))]
;        [else (cond [(not (empty? t2)) t2] [else (nop)])])) 
(define-peg bloco (and wordSeparator (name t1 declaracao) wordSeparator pointvirg wordSeparator (name t2 comando)) (blk t1 t2))
;condicionais

(provide peg-rule:condicional)

(struct ifP (condicao corpo) #:transparent)
(struct ifElse (condicao corpoIf corpoElse) #:transparent)
(struct condicional (U ifP ifElse) #:transparent)


(define-peg newIf (and wordSeparator
                  "if" spaces (name condicao boolExp) wordSeparator (or (and "{" wordSeparator (name corpoIf (or bloco comando)) wordSeparator "}") ;or blocoF bloco comandoF comando
                                                                 (name corpoIf cmdUnit)) wordSeparator
                   (? (and "else" wordSeparator (or (and "{" wordSeparator (name corpoElse (or bloco comando)) wordSeparator "}") (name corpoElse cmdUnit))))) ;or blocoF bloco comandoF comando
  (cond [corpoElse (ifElse condicao corpoIf corpoElse)] [else (ifP condicao corpoIf)]))




;(define-peg ifElse (and wordSeparator
;                  "if" spaces (name condicao boolExp) spaces (or (and "{" wordSeparator (name corpoIf (or bloco comando)) wordSeparator "}")
;                                                                 (name corpoIf cmdUnit)) wordSeparator
;                   "else" spaces (or (and "{" wordSeparator (name corpoElse (or bloco comando)) wordSeparator "}") (name corpoElse cmdUnit)))
;  (ifElse condicao corpoIf corpoElse))


;(define-peg if (and wordSeparator
;                  "if" spaces (name condicao boolExp) spaces (or (and "{" wordSeparator (name corpo (or bloco comando)) wordSeparator "}")
;                                                                 (name corpo cmdUnit)))
;  (ifP condicao corpo))

(define-peg condicional newIf)
  ;(or ifElse if))

(struct if (cond then else)#:transparent)
(struct print(a)#:transparent)
(provide print print? print-a)


(provide if if? if-cond if-then if-else)


;loop

(provide peg-rule:loop)

(struct whileDo (condicao corpo)  #:transparent)

(define-peg loop (and
"while" spaces (name condicao boolExp) wordSeparator "do" spaces "{" wordSeparator (name corpo (or bloco comando)) wordSeparator "}") ;or blocoF bloco comandoF comando
  (whileDo condicao corpo))

(struct loop (cond corpo) #:transparent)
(provide loop)

(struct act (exp a) #:transparent)

(struct calAtuals (idt a) #:transparent)
(struct cal (idt) #:transparent)
(provide exit? exit-a)
(provide comandoConv)
(provide blk)
(define  (transAtual a)
  (match a
    [(list b) (expConv b)]
    [(list a b ...) (act (expConv a) (transAtual b))]))





(define (comandoConv exp)
	(match exp
	[(ifP condicao corpo) (if (boolConv condicao) (comandoConv corpo) (nop))]
	[(ifElse condicao then else) (if (boolConv condicao) (comandoConv then) (comandoConv else))]
	[(whileDo condicao corpo) (loop (boolConv condicao)(comandoConv corpo))]
	[(seq a b)(seq (comandoConv a)(comandoConv b))]
	[(prnt a) (print (cond [(boolExp? a) (boolConv a)] [else (aritConv a)] ))]
	[(choice a b)(choice (comandoConv a) (comandoConv b))]
	[(exit a)(exit (if (boolExp? a) (boolConv a) (aritConv a) ))]
	[(atribution a b) (atribConv (atribution a b)) ]
	[(blk a b) (blk (atribConv a) (comandoConv b))]
        [(call a #f) (cal a)]
        [(call a b) (calAtuals a (transAtual b))] ))
