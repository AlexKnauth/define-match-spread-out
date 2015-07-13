#lang racket/base

(provide define*)

(require racket/match
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
  (struct define*-info (n args bodys) #:prefab)
  ;; n : Natural             ; This might be generalized later to a full arity+keywords
  ;; args : (Listof Syntax)
  ;; bodys : (Listof Syntax)
  (struct define*-info-lst ([lst #:mutable]) #:prefab)
  ;; lst : (Listof define*-info)
  ;; define*-info-lst-add! : define*-info-lst define*-info -> Void
  (define (define*-info-lst-add! dil di)
    (match-define (define*-info-lst lst) dil)
    (set-define*-info-lst-lst! dil (append lst (list di))))
  ;; define*-info-lst -> Syntax
  (define (define*-info-lst->match-lambda-ish dil)
    (match-define (define*-info-lst lst) dil)
    (define ns (remove-duplicates (map define*-info-n lst)))
    (define/syntax-parse (case-lambda-clause ...)
      (for/list ([n (in-list ns)])
        (define dis
          (for/list ([di (in-list lst)] #:when (= (define*-info-n di) n))
            di))
        (cond
          [(empty? dis)
           (error 'define* "this should never happen, internal error, bug")]
          [else
           (define/syntax-parse arg-ids
             (generate-temporaries (define*-info-args (first dis))))
           (define/syntax-parse (arg-pats ...) (map define*-info-args dis))
           (define/syntax-parse (bodys ...) (map define*-info-bodys dis))
           #'[arg-ids
              (match* arg-ids
                [arg-pats . bodys]
                ...)]])))
    #'(case-lambda case-lambda-clause ...))
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
      [(define* (f:id arg-pat:expr ...) body:expr ...)
       (match-define (define*-binding id info-lst) (syntax-local-value #'f))
       (define arg-pats (syntax->list #'(arg-pat ...)))
       (define n (length arg-pats))
       (define bodys (syntax->list #'(body ...)))
       (define di (define*-info n arg-pats bodys))
       (define*-info-lst-add! info-lst di)
       (define/syntax-parse f* (syntax-local-introduce id))
       (define/syntax-parse match-lambda-ish
         (define*-info-lst->match-lambda-ish info-lst))
       #'(set-box! f* (procedure-rename match-lambda-ish 'f))]
      )))

(define-simple-macro (define* (f:id arg-pat:expr ...) body:expr ...+)
  (begin
    (maybe-define-define*-binding f)
    (define*-add! (f arg-pat ...) body ...)))

