#lang racket/base

(require rackunit
         "../main.rkt"
         "test.rkt")

(define* (f x y z) (* x y z))

(check-equal? (f #t) 2)
(check-equal? (f 1 2 3) 6)

(define* (+ (vector a ...) (vector b ...))
  (apply vector-immutable (map + a b)))

(check-equal? (+ #[1 2 3] #[4 5 6] #[7 8 9]) #[12 15 18])

