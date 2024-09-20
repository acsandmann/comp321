#lang racket

(define (curry2 f)
  (lambda (x)
    (lambda (y)
      (f x y)))
  )

(define add5 ((curry2 +) 5))
(add5 1)

(define lst '(1 2 3))

(((curry list) 1 2))