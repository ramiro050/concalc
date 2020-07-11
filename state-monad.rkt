#lang racket
(require data/functor
         data/applicative
         data/monad)

(provide (struct-out state)
         run-state
         eval-state
         exec-state
         stack-state
         (all-from-out data/monad)
         (all-from-out data/applicative)
         (all-from-out data/functor))

(define (<*> st-f st)
  (do [f <- st-f]
    [x <- st]
    (pure (f x))))

(define (flip-<*> st st-f)
  (<*> st-f st))

(define (>> m k)
  (chain (lambda (_) k) m))

(define (sequenceM lstM)
  (foldr >> (pure '()) lstM))

(struct state (app)
  #:transparent
  #:methods gen:functor
  [(define (map f st)
     (state
      (lambda (s) (match ((state-app st) s)
                    [(cons a new-s) (cons (f a) new-s)]))))]
  #:methods gen:applicative
  [(define (pure _ a)
     (state (lambda (s) (cons a s))))
   (define (apply st-f sts)
     (foldl flip-<*> st-f sts))]
  #:methods gen:monad
  [(define (chain f st)
     (state
      (lambda (s) (match ((state-app st) s)
                    [(cons a new-s)
                     ((state-app (f a)) new-s)]))))])

(define (run-state st init)
  ((state-app st) init))

(define (eval-state st init)
  (car (run-state st init)))

(define (exec-state st init)
  (cdr (run-state st init)))

(define (stack-state st init)
  (cadr (run-state st init)))