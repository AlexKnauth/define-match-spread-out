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

(begin-for-syntax
  ;; (define*-binding box-id clauses next-id
  ;; box-id: Identifier ; points to a box at runtime containing the function
  ;; clauses: Syntax ; a syntax list (at compile time) containing the match*-case-lambda clauses
  ;; next-id: Identifier ; points to the next identifier, which is or will be the next define*-binding
  ;;   if the next define*-binding exists, then this one il old
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
         (with-syntax ([id (generate-temporary #'f)]
                       [next (generate-temporary #'f)])
           #'(begin
               (define id (box (let ([f (case-lambda)]) f)))
               (define-syntax f
                 (define*-binding (quote-syntax id) (quote-syntax ()) (quote-syntax next))))))]))

(define-syntax define*-add!
  (lambda (stx)
    (syntax-parse stx
      [(define* (f:id . args) body:expr ...)
       (match-define (define*-binding
                       (app syntax-local-introduce id*)
                       (app syntax-local-introduce clauses*)
                       (app syntax-local-introduce next*))
         (get-define*-binding #'f))
       (define/syntax-parse id id*)
       (define/syntax-parse (cls ...) clauses*)
       (define/syntax-parse next next*)
       (define/syntax-parse next-next (generate-temporary #'f))
       (define/syntax-parse new-clause #'[args body ...])
       (define/syntax-parse new-clauses #'(cls ... new-clause))
       (define/syntax-parse f-box id*)
       (define/syntax-parse match-lambda-ish
         (clauses->match-lambda-ish #'new-clauses))
       #'(begin
           (define-syntax next
             (define*-binding (quote-syntax id) (quote-syntax new-clauses) (quote-syntax next-next)))
           (set-box! f-box (procedure-rename match-lambda-ish 'f)))]
      )))

(define-simple-macro (define* (f:id . args) body:expr ...+)
  (begin
    (maybe-define-define*-binding f)
    (define*-add! (f . args) body ...)))

