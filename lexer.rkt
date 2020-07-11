#lang racket
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(provide port->tokens)

(define-tokens values (ID OP))
(define-empty-tokens primitives (PRGM BIND IF THEN ELSE))

(define-lex-abbrevs
  (ws (:+ whitespace))
  (comm (:seq "--" (:* (:- any-char #\newline)) #\newline))
  (real (:seq (:* numeric)
              (:? ".")
              (:+ numeric)))
  (id (:seq lower-case
            (:* (:or lower-case numeric)))))

(define tokenize
  (lexer-src-pos
   [ws (void)]
   [comm (void)]
   [real (token-OP `(op-push ,(string->number lexeme)))]
   [(:or ":") (token-PRGM)]
   ["->" (token-BIND)]
   ["nothing" (token-OP 'op-nothing)]
   ["swap" (token-OP 'op-swap)]
   ["dup" (token-OP 'op-dup)]
   ["if" (token-IF)]
   ["else" (token-ELSE)]
   ["then" (token-THEN)]
   ["drop" (token-OP 'op-drop)]
   ["sqrt" (token-OP 'op-sqrt)]
   ["sqr" (token-OP 'op-sqr)]
   ["exp" (token-OP 'op-exp)]
   ["1/" (token-OP 'op-1/)]
   ["+" (token-OP 'op-+)]
   ["-" (token-OP 'op--)]
   ["/" (token-OP 'op-/)]
   ["*" (token-OP 'op-*)]
   ["^" (token-OP 'op-^)]
   [">" (token-OP 'op->)]
   ["<" (token-OP 'op-<)]
   ;; Built-in operators must come before id to break ties
   [id (token-ID (string->symbol lexeme))]
   [(eof) eof]))

(define (port->tokens-rec ip)
  (let* ([p-token (tokenize ip)]
         [token-val (position-token-token p-token)])
    (cond
      [(eof-object? token-val) '()]
      [(void? token-val) (port->tokens-rec ip)]
      [else (cons p-token (port->tokens-rec ip))])))

(define (port->tokens ip)
  (port-count-lines! ip)
  (port->tokens-rec ip))
