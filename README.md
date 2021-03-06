define-match-spread-out [![Build Status](https://travis-ci.org/AlexKnauth/define-match-spread-out.png?branch=master)](https://travis-ci.org/AlexKnauth/define-match-spread-out)
===

documentation: http://docs.racket-lang.org/define-match-spread-out/index.html

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
  (match*-case-lambda
    [[#t] 2]
    [[#f] 3]
    [[_] 0]
    [[x y] x]))
```
Or, using just `case-lambda` and `match` variants:
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
