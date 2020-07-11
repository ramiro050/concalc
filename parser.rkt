#lang racket
(require "lexer.rkt")
(require data/monad
         data/applicative)
(require megaparsack
         (only-in megaparsack/parser-tools/lex
                  parse-tokens
                  token/p))

(provide parse)

;; GRAMMAR
;; PRGM : (FUNC | EXPR)+
;; EXPR : op | id | COND
;; FUNC : ":" EXPR+ "->" id
;; COND : "if" EXPR+ "else" EXPR+ "then"

(define cond/p
  (syntax/p
   (do (token/p 'IF)
     [es-true <- (many+/p expr/p)]
     (token/p 'ELSE)
     [es-false <- (many+/p expr/p)]
     (token/p 'THEN)
     (pure `(if-then-else ,es-true ,es-false)))))

(define op/p
  (syntax/p
   (do [e <- (token/p 'OP)]
     (pure `(expr-op ,e)))))

(define id/p
  (syntax/p
   (do [e <- (token/p 'ID)]
     (pure `(expr-id ,e)))))

(define func/p
  (syntax/p
   (do (token/p 'PRGM)
     [body <- (many+/p expr/p)]
     (token/p 'BIND)
     [name <- (token/p 'ID)]
     (pure `(func ,name ,body)))))

(define expr/p
  (syntax/p (or/p op/p id/p cond/p)))

(define prgm/p
  (syntax/p
   (do [p <- (many+/p (or/p func/p expr/p))]
     (pure `(prgm ,p)))))

(define (parse src ip)
  (parse-result! (parse-tokens prgm/p (port->tokens ip) src)))