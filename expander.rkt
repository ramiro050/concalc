#lang racket
(require "state-monad.rkt" "primitives.rkt")

(provide (except-out (all-from-out racket) #%module-begin)
         (rename-out [module-begin #%module-begin])
         (all-from-out "primitives.rkt")
         expr-op
         expr-id
         if-then-else
         func)

(define-syntax (expr-op stx)
  (syntax-case stx ()
    [(_ op) #'op]))

(define-syntax (expr-id stx)
  (syntax-case stx ()
    [(_ id) #'(run-word (quote id))]))

(define-syntax (if-then-else stx)
  (syntax-case stx ()
    [(_ (true-clause ...) (false-clause ...))
     #'(do [p <- op-pop]
         (cond
           [(zero? p) (do false-clause ...)]
           [else (do true-clause ...)]))]))

(define-syntax (func stx)
  (syntax-case stx ()
    [(_ name (body ...))
     #'(insert-in-env (quote name) (do body ...))]))

(define-syntax (module-begin stx)
  (syntax-case stx ()
    [(_ (_ (exprs ...)))
     #'(#%module-begin
        (provide final-state)
        (define final-state
          (do exprs ...))
        (stack-state final-state (cons '() '())))]))