#lang racket
(require syntax/strip-context)
(require "parser.rkt")

(provide (rename-out [concalc-read read]
                     [concalc-read-syntax read-syntax]))

(define (concalc-read in)
  (syntax->datum
   (concalc-read-syntax #f in)))

(define (concalc-read-syntax src in)
  (strip-context
   #`(module anything "expander.rkt"
       #,(parse src in))))