#lang scribble/manual

@(require scribble-code-examples
          (for-label racket/base
                     racket/contract/base
                     define-match-spread-out
                     defpat/match-case-lambda
                     ))

@title{define-match-spread-out}

source code: @url{https://github.com/AlexKnauth/define-match-spread-out}

@defmodule[define-match-spread-out]{
This file provides the @racket[define*] form, which allows function
definitions to be spread out across a file.
}

@defform[(define* (f-id args) body ...)
         #:grammar
         ([args
           (code:line arg-pat ...)
           (code:line @#,racketparenfont{.} rest-id)
           (code:line arg-pat ... @#,racketparenfont{.} rest-id)])]{
A version of @racket[define] that allows definitions to be spread
across a file. Each @racket[arg-pat] can be an arbitrary
@racket[match] pattern, and if the given argument values don't match
the patterns in the first form in the file, it tries matching them
against the patterns in second form, and so on.

The patterns of all of the @racket[define*] forms in the file so far
are collected into a single @racket[match*-case-lambda] form.

@code-examples[#:lang "racket/base" #:context #'here]{
(require define-match-spread-out)
(define* (f #t) 2)
(f #t)
(define* (f #f) 3)
(f #t)
(f #f)
(define* (f _) 0)
(f #t)
(f "non-boolean stuff")
(define* (f x y) x)
(f #t)
(f "two" "arguments")
}}

