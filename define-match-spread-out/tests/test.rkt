#lang racket/base

(require rackunit
         racket/match
         "../main.rkt"
         )

(define* (f #t) 2)
(define* (f #f) 3)
(define* (f  _) 0)
(define* (f x y) x)

(check-equal? (f #t) 2)
(check-equal? (f #f) 3)
(check-equal? (f 5) 0)
(check-equal? (f 1 2) 1)

;; define*-ing + in terms of add1 and sub1
(define* (+) 0)
(define* (+ a) a)
(define* (+ 0 a) a)
(define* (+ (? exact-positive-integer? a) b)
  (+ (sub1 a) (add1 b)))
(define* (+ (? exact-integer? a) b)
  (+ (add1 a) (sub1 b)))
(define* (+ a (? exact-integer? b)) (+ b a))
(define* (+ a b . rst)
  (apply + (+ a b) rst))

(check-equal? (+ 3000 140 1.59) 3141.59)
(check-equal? (+ -3000 -140 -1.59) -3141.59)

