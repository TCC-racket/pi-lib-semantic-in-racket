#lang racket

(provide env env? env-add)

(struct env (realEnv positionFree) #:transparent)

(define newEnv (env (hash) 0))



