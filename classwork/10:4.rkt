#lang racket

(define (max-abs n . rst)
  (foldr (lambda (n m) (max (abs n) m)) (abs n) rst))

(max-abs 1 -3 -6 5)

;;(provide
;; (contract-out
;;  [max-abs (->* (real?) () #:rest (listof real?) real?)]))

(define (real-sqrt x) (sqrt x))

;;(provide
;; (contract-out [real-sqrt (-> (>=/c 1) any/c?)])
;; )

