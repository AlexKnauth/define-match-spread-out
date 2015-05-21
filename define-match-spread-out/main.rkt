#lang racket/base

(provide define*)

(require racket/match
         (for-syntax racket/base
                     racket/list
                     racket/match
                     syntax/parse
                     ))

(begin-for-syntax
  ;; intro : Syntax -> Syntax
  (define intro (make-syntax-introducer))
  ;; f->f* : Identifier -> Identifier
  ;; f is the id suplied to (define* (f arg ...) body ...)
  ;; f->f* produces the id that is, or will be, defined as (define-syntax f* the-define*-info-lst)
  (define (f->f* f)
    (syntax-local-introduce (intro (syntax-local-introduce f))))
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

(define-syntax define*
  (lambda (stx)
    (syntax-parse stx
      [(define* (f:id arg-pat:expr ...) body:expr ...)
       #:with f* (f->f* #'f)
       (define v (syntax-local-value #'f* (λ () #f)))
       (define arg-pats (syntax->list #'(arg-pat ...)))
       (define n (length arg-pats))
       (define bodys (syntax->list #'(body ...)))
       (define di (define*-info n arg-pats bodys))
       (cond
         [(define*-info-lst? v)
          (define*-info-lst-add! v di)
          (define/syntax-parse match-lambda-ish
            (define*-info-lst->match-lambda-ish v))
          #'(set! f match-lambda-ish)]
         [else
          (define dil
            (define*-info-lst (list di)))
          (define/syntax-parse match-lambda-ish
            (define*-info-lst->match-lambda-ish dil))
          #`(begin (define-syntax f* #,dil)
                   (define f match-lambda-ish))])])))

