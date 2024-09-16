#lang racket

(define lst (list 1 2 3 4))

(define (add-two lst)
  (if (empty? lst) lst
      (cons (+ (first lst) 2) (add-two (rest lst)))
      )
  )
  
                                     
(add-two lst)