#lang typed/racket

;; (: factorial (-> Natural Natural))

(define (factorial [n : Natural]) : Natural
  (if (zero? n)
      1
      (* n (factorial (- n 1)))
))

(struct nothing ())

(struct pt ([x : Real] [y : Real]))

(: distance (-> pt pt Real))

(define (distance p1 p2)
  (sqrt (+ (sqr (- (pt-x p2) (pt-x p1)))
           (sqr (- (pt-y p2) (pt-y p2))))))

(define dist-from-origin (curry distance (pt 0 0)))

(dist-from-origin (pt 2 2))

(lambda ([x : Real]) (if (> 0 x) "yes" (+ x 1)))
;; : (-> Real (U String Zero))

(: flexible-length (-> (U String (Listof Any)) Integer))
(define (flexible-length str-or-lst)
  (if (string? str-or-lst)
      (string-length str-or-lst)
      (length str-or-lst)
      )
  )