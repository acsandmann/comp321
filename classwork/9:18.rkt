#lang racket

(define (avg lst)
(/ (+ lst) (length lst)))

(apply + '(1 2 3))

(define (all-titlecase lst)
  (filter (lambda (str) (string=? (string-titlecase str) str))
  lst))

(all-titlecase '("hello", "Hello"))