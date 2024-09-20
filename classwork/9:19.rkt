#lang racket

(define (xor lst)
  (= (length (filter (lambda (x) x) lst)) 1)
  )

(xor '(1 0 0 0 0))

(xor '(#t #f #f))

(define (my-filter f lst)
  (cond ((empty? lst) empty)
        ((f (first lst))
         (cons (first lst) (my-filter f (rest lst))))
        (else (my-filter f (rest lst)))))

(my-filter (lambda (x) (> x 2)) '(1 2 3))

((compose sqrt add1) 24)

;; write a function called (compose-all f1 f2 .. fn) that returns the composition of all f1 to fn, ie. (f1 (f2 ... (fn x)))
(define (compose-all . fs)
  (foldr (lambda (f acc)
           (lambda (x) (f (acc x))))
         (lambda (x) x)
         fs))

(define composed-fn (compose-all add1 add1 add1))

(composed-fn 10)

(= (pred x) #t)

  