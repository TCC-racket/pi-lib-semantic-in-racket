#lang racket

(require peg/peg)
(require "Comando.rkt")
(require "atribuicao.rkt")
(require "espacos.rkt")
(require "Reservadas.rkt")

;(provide peg-rule:programa)

(struct proc (idt args corpo) #:transparent)
(struct procSeq (proc seq) #:transparent)
(struct idt (nome) #:transparent)
(struct pgrm (ident clauses) #:transparent)
(struct clauses (vars consts inits procs) #:transparent)
(struct blkP (cmds) #:transparent)

;(define-peg processo (and proc wordSeparator (name t1 token) wordSeparator "{" wordSeparator (name t2 comando) wordSeparator "}") (proc t1 t2))

;(define-peg token (and
;		(* (or (range #\a #\z) "(" ")" (range #\A #\Z) ))))

;(define-peg inicializacaoCL (and (? (name t1 inicializacao)) wordSeparator ) (inicializacaoOp t1));(name t2 seqProcesso))(if t1 (inicializacaoOp t1 t2) t2))
;(define-peg seqProcesso (and (name t1 processo) (? wordSeparator (name t2 seqProcesso))) (if t2 (procSeq t1 t2) t1))

;(define-peg comentario (*(and "---" spaces specialtoken newLine)))
;(define-peg specialtoken (and
;		(* (or (range #\a #\z) space (range #\A #\Z) ))))
;(define-peg anything (and
;		(* (any-char))))

(define-peg ident (name nome (and  
                          (or (range #\a #\z) (range #\A #\Z))
                          (* (and (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9))
                              (* (or "_" "-")
                                 (or (range #\a #\z) (range #\A #\Z))
                                 (and (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)))))))) (idt nome) )

; Lista de argumentos para proc
(define-peg variableAUX (and wordSeparator
                            (name t1 variable)
                            (? wordSeparator virg wordSeparator
                               (name t2 variableAUX)))
  (cond [t2 (cons (idt t1) t2)] [else (list (idt t1))]))

(define-peg variableList (? (and wordSeparator
                             (name t1 variable)
                             (? wordSeparator virg wordSeparator (name t2 variableAUX))))
  (cond [t2 (cons (idt t1) t2)] [else (idt t1)]))
                   
(define-peg programa (and wordSeparator "("
                      wordSeparator module wordSeparator
                      (name t1 ident) wordSeparator
                      (name t2 clausesR) wordSeparator
                      end ")") (pgrm t1 t2))

;R de "rule" | "regra"
(define-peg clausesR (and wordSeparator                          
                         (name t1 variavel) wordSeparator
                         (name t2 constante) wordSeparator
                         (name t3 inicializacao) wordSeparator
                         (name t4 procedure) wordSeparator
                         ) (clauses t1 t2 t3 t4))

(define-peg procedure (or procSeq procedureUnit))

(define-peg procedureUnit (and proc wordSeparator
                      (name t1 ident)
                      wordSeparator "(" wordSeparator
                      (name t2 variableList) wordSeparator ")"
                      wordSeparator "{" wordSeparator
                      (name t3 (or bloco blocoPRC)) wordSeparator
                      "}" wordSeparator) (proc t1 t2 t3))

(define-peg procSeq(and wordSeparator (name t1 procedureUnit) (? wordSeparator (name t2 procSeq))) (cond [t2 (procSeq t1 t2)] [else t1]))

(define-peg blocoPRC (and (name t1 comando)) (blkP t1))


(define (var->list a)
  (match a
    [#f '()]
    [(variavelCL i) '(i)]
    [(? list l) (map (lambda(x) (variavelCL-name x)) l)]))
(define (const->list a)
  (match a
    [#f '()]
    [(constanteCL i) '(i)]
    [(? list l) (map (lambda(x) (constanteCL-name x))l)]))

(define (init->list a)
  (match a
    [#f '()]
    [a a]))




(struct prc (ident bloco) #:transparent)
(struct prcFormals (ident args bloco) #:transparent)

(struct par (ident) #:transparent)
(struct for (ident rest) #:transparent)


(define (constroiGlobais v c i)
  (match (list v c i)
    [(list (list) (list a) (list (initCL a value))) 
     (cns a value)]
    [(list(list a)(list) (list(initCL a value)))
     (ref a value)]
     
    [(list (list-no-order a b ...) (? list? k) (list-no-order (initCL a value) c ...))
     (dec (ref a value) (constroiGlobais b k c))]
    [(list (? list? k) (list-no-order a b ...) (list-no-order (initCL a value) c ...))
     (dec (cns a value) (constroiGlobais b k c))]
    [(list (list)(list)(list(initCL a value)c ...))
     (raise "Mais inicializacoes do que variaveis ou constantes")]
    ))

  

(define (destroyClauses a)
  (match a
    [(clauses #f #f #f p) (let ([procs (progConv p)]) (blk procs (call (idt "main"))))]
    [(clauses var const initCL procs)
      (let ([decGlobais (constroiGlobais (var->list var) (const->list const) (init->list initCL))]
            [semGlobais (destroyClauses (clauses #f #f #f procs))])
            (blk decGlobais semGlobais))]))



(define (progConv p)
  (match p
    [(pgrm name (? clauses? a)) (destroyClauses a)]
    [(procSeq a b) (dec (progConv a) (progConv b))]
    [(proc ident (idt #f) bloco) (prc ident (comandoConv bloco))]
    [(proc ident args bloco) (prcFormals ident (progConv args) (comandoConv bloco))]
    [(? idt? x) (par x)]
    [(list a) (par a)] ; argumento
    [(list a b ...) (for (par a) (progConv b))])) ;argumentos






