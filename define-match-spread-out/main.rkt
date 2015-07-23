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
  (struct define*-binding (id define*-info)
    #:property prop:procedure
    (λ (this stx)
      (define/syntax-parse id (define*-binding-id this))
      (define proc (set!-transformer-procedure (make-variable-like-transformer #'(unbox id))))
      (proc stx)))
  (struct define*-info-lst ([lst #:mutable]) #:prefab)
  ;; lst : (Listof Syntax) ; a list of clauses for match*-case-lambda
  ;; define*-info-lst-add! : define*-info-lst define*-info -> Void
  (define (define*-info-lst-add! dil cls)
    (match-define (define*-info-lst lst) dil)
    (set-define*-info-lst-lst! dil (append lst (list cls))))
  ;; define*-info-lst -> Syntax
  (define (define*-info-lst->match-lambda-ish dil)
    (match-define (define*-info-lst lst) dil)
    (define/syntax-parse (match*-case-lambda-clause ...)
      lst)
    #'(match*-case-lambda match*-case-lambda-clause ...))
  )

(define-syntax maybe-define-define*-binding
  (syntax-parser
    [(maybe-def f:id)
     (if (syntax-local-value #'f (λ () #f))
         (syntax-property
          #'(define-values () (values))
          'disappeared-binding (list (syntax-local-introduce #'f)))
         (with-syntax ([id (generate-temporary #'f)])
           #'(begin
               (define id (box (let ([f (case-lambda)]) f)))
               (define-syntax f (define*-binding #'id (define*-info-lst '()))))))]))

(define-syntax define*-add!
  (lambda (stx)
    (syntax-parse stx
      [(define* (f:id . args) body:expr ...)
       (match-define (define*-binding id info-lst) (syntax-local-value #'f))
       (define clause #'[args body ...])
       (define*-info-lst-add! info-lst clause)
       (define/syntax-parse f* (syntax-local-introduce id))
       (define/syntax-parse match-lambda-ish
         (define*-info-lst->match-lambda-ish info-lst))
       #'(set-box! f* (procedure-rename match-lambda-ish 'f))]
      )))

(define-simple-macro (define* (f:id . args) body:expr ...+)
  (begin
    (maybe-define-define*-binding f)
    (define*-add! (f . args) body ...)))

