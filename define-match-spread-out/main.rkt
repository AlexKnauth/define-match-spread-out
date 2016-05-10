#lang racket/base

(provide define*)

(require racket/match
         defpat/match-case-lambda
         syntax/parse/define
         (for-syntax racket/base
                     racket/list
                     racket/match
                     syntax/parse
                     racket/syntax
                     unstable/syntax
                     ))

;; This file defines a `define*` macro that allows definitions to be
;; spread across a file. To do this, it generates a "next" identifier.

;; The "next" identifier will either be unbound or will be bound with
;; a `define-syntax` to a compile-time value. If that value exists, it
;; will be an instance of the `define*-binding` struct.

;; The `define*-binding` itself contains a "next" id, which points to
;; the next `define*-binding` instance after itself. If this next
;; "next" id is bound, then the other contents of the current
;; `define*-binding` instance are old and should be ignored. Instead
;; it repeats the process with the next "next" id, which points to the
;; next `define*-binding` instance.

(begin-for-syntax
  ;; (define*-binding box-id clauses next-id)
  ;; box-id: Identifier ; points to a box at runtime containing the function
  ;; clauses: Syntax ; a syntax list (at compile time) containing the match*-case-lambda clauses
  ;; next-id: Identifier ; points to the next identifier, which is or will be the next define*-binding
  ;;   if the next define*-binding exists, then this one is old
  (struct define*-binding (box-id clauses next-id)
    #:property prop:procedure
    (λ (this stx)
      (define/syntax-parse box-id (define*-binding-box-id this))
      (define proc (set!-transformer-procedure (make-variable-like-transformer #'(unbox box-id))))
      (proc stx)))
  ;; clauses->match-lambda-ish : Syntax -> Syntax
  (define (clauses->match-lambda-ish clauses)
    (define/syntax-parse (match*-case-lambda-clause ...)
      clauses)
    #'(match*-case-lambda match*-case-lambda-clause ...))
  ;; get-define*-binding : Identifier -> define*-binding
  (define (get-define*-binding id)
    (follow-define*-bindings (syntax-local-value id)))
  ;; follow-define*-bindings : define*-binding -> define*-binding
  (define (follow-define*-bindings v)
    (match v
      [(define*-binding box-id clauses next-id)
       (define v* (syntax-local-value (syntax-local-introduce next-id) (λ () #f)))
       (cond [v* (follow-define*-bindings v*)]
             [else v])]))
  )

(define-syntax maybe-define-define*-binding
  (syntax-parser
    [(maybe-def f:id)
     (if (syntax-local-value #'f (λ () #f))
         (syntax-property
          #'(define-values () (values))
          'disappeared-binding (list (syntax-local-introduce #'f)))
         (with-syntax ([box-id (generate-temporary #'f)]
                       [next (generate-temporary #'f)])
           #'(begin
               (define box-id (box (let ([f (case-lambda)]) f)))
               (define-syntax f
                 (define*-binding (quote-syntax box-id) (quote-syntax ()) (quote-syntax next))))))]))

(define-syntax define*-add!
  (lambda (stx)
    (syntax-parse stx
      [(define* (f:id . args) body:expr ...)
       (match-define (define*-binding
                       (app syntax-local-introduce box-id*)
                       (app syntax-local-introduce clauses*)
                       (app syntax-local-introduce next*))
         (get-define*-binding #'f))
       (define/syntax-parse box-id box-id*)
       (define/syntax-parse (cls ...) clauses*)
       (define/syntax-parse next next*)
       (define/syntax-parse next-next (generate-temporary #'f))
       (define/syntax-parse new-clause #'[args body ...])
       (define/syntax-parse new-clauses #'(cls ... new-clause))
       (define/syntax-parse match-lambda-ish
         (clauses->match-lambda-ish #'new-clauses))
       #'(begin
           (define-syntax next
             (define*-binding (quote-syntax box-id) (quote-syntax new-clauses) (quote-syntax next-next)))
           (set-box! box-id (procedure-rename match-lambda-ish 'f)))]
      )))

(define-simple-macro (define* (f:id . args) body:expr ...+)
  (begin
    (maybe-define-define*-binding f)
    (define*-add! (f . args) body ...)))

