#lang racket
(require "state-monad.rkt")

(provide op-+
         op--
         op-/
         op-*
         op->
         op-<
         op-sqr
         op-swap
         op-dup
         op-nothing
         run-word
         insert-in-env
         (rename-out
          [push op-push]
          [pop op-pop]))

(define (safe-car lst)
  (cond
    [(null? lst) 0]
    [else (car lst)]))

(define (safe-cdr lst)
  (cond
    [(null? lst) lst]
    [else (cdr lst)]))

(define pop
  (state
   (lambda (s)
     (match s
         [(cons stk env)
          (cons (safe-car stk)
                (cons (safe-cdr stk) env))]))))

(define (push a)
  (state
   (lambda (s)
     (match s
       [(cons stk env)
        (cons '() (cons (cons a stk) env))]))))

(define (insert-assoc k v lst)
  (cond
    [(null? lst) (list (cons k v))]
    [else (match (car lst)
            [(cons old-k _)
             (cond [(equal? k old-k) (cons (cons k v) (cdr lst))]
                   [else (cons (car lst)
                               (insert-assoc k v (cdr lst)))])])]))

(define (insert-in-env k v)
  (state
   (lambda (s)
     (match s
       [(cons stk env)
        (cons '() (cons stk (insert-assoc k v env)))]))))

(define (get-assoc k lst)
  (let ([v (assoc k lst)])
    (cond
      [(not v) (raise-syntax-error k "undefined word" k)]
      [else (cdr v)])))

(define (get-from-env k)
  (state
   (lambda (s)
     (match s
       [(cons stk env)
        (cons (get-assoc k env) (cons stk env))]))))

(define (app-1 f)
  (do [x <- pop]
    (push (f x))))

(define (app-2 f)
  (do [x <- pop]
    [y <- pop]
    (push (f y x))))

(define op-nothing (pure '()))

(define op-swap
  (do [x <- pop]
    [y <- pop]
    (push x)
    (push y)))

(define op-dup
  (do [x <- pop]
    (push x)
    (push x)))

(define (run-word name)
  (do [word <- (get-from-env name)]
    word))

(define op-+ (app-2 +))
(define op-- (app-2 -))
(define op-/ (app-2 /))
(define op-* (app-2 *))
(define op-sqr (app-1 sqr))

(define (bool->int b)
  (cond [b 1]
        [else 0]))

(define (comp f)
  (do [x <- pop]
    [y <- pop]
    (push (bool->int (f y x)))))

(define op-> (comp >))
(define op-< (comp <))