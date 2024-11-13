#lang plait

(require "interpreter.rkt")

(define test1 (plusE (numE 3) (boolE #t))) ; Should return (numV 4)
(define test2 (plusE (numE 3) (boolE #f))) ; Should return (numV 3)
(define test3 (plusE (boolE #t) (boolE #f))) ; Should return (numV 1)
(define test4 (plusE (boolE #t) (boolE #t))) ; Should return (numV 2)

(define test5 (cndE (numE 1) (numE 42) (numE 0)))  ; Should return (numV 42)
(define test6 (cndE (numE 0) (numE 42) (numE -1))) ; Should return (numV -1)

(define test7 (cndE (numE -5) (numE 1) (numE 0)))


(test (interp test1 mt-env) (numV 4))
(test (interp test2 mt-env) (numV 3))
(test (interp test3 mt-env) (numV 1))
(test (interp test4 mt-env) (numV 2))

(test (interp test5 mt-env) (numV 42))
(test (interp test6 mt-env) (numV -1))

(test/exn (interp test7 mt-env) "if: Condition value cannot be less than 0")