define-match-spread-out
===

A racket library that allows you to write code like this:
```racket
(define* (f #t) 2)
(define* (f #f) 3)
(define* (f  _) 0)
(define* (f x y) x)
```
That would be roughly equivalent to
```racket
(define f
  (case-lambda
    [(tmp)
     (match tmp
       [#t 2]
       [#f 3]
       [_  0])]
    [(tmp1 tmp2)
     (match* (tmp1 tmp2)
       [(x y) x])]))
```
