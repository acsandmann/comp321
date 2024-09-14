#lang racket
(require rackunit)

;; question 1

(define (string-multiply s n)
  (if (<= n 0)
      ""
      (string-append s (string-multiply s (- n 1)))))

(check-equal? (string-multiply "dog" 5) "dogdogdogdogdog")

;; question 2, part 1

(define (p1 s n)
  (define (h i)
    (when (<= i n)
      (displayln (make-string (+ (* 2 i) 1) (string-ref s 0)))
      (h (+ i 1))))
  (h 0))

(p1 "x" 4)

;; question 2, part 2

(define (p3 s n)
  (define (h i)
    (when (<= i n)
      (let ((spaces (- n i)))
        (displayln (string-append (make-string spaces #\space)
                                  (make-string (+ (* 2 i) 1) (string-ref s 0)))))
      (h (+ i 1))))
  (h 0))

(p3 "x" 4)

;; question 3

(define (alphabetized? lst)
  (cond
    [(null? (cdr lst)) #t]
    [(string<=? (car lst) (cadr lst)) (alphabetized? (cdr lst))]
    [else #f]))

(check-true (alphabetized? (list "apple" "banana" "carrot"))) ; test #t

(check-false (alphabetized? (list "zebra" "yak" "xenomorph"))) ; test #f

;; question 4

(define (power x y) ; basic poewr fnx
  (if (= y 0)
      1
      (* x (power x (- y 1)))))

#|
power(3, 4) = 3 * power(3, 3)
power(3, 3) = 3 * power(3, 2)
power(3, 2) = 3 * power(3, 1)
power(3, 1) = 3 * power(3, 0)
power(3, 0) = 1
final = 3 * 3 * 3 * 3 = 81
|#

(define (tail-power x y) ; power but tail optimized
  (define (helper x y acc)
    (if (= y 0)
        acc
        (helper x (- y 1) (* acc x))))
  (helper x y 1))

(check-equal? (power 2 3) 8) ; test power()
(check-equal? (tail-power 2 3) 8) ; test tail-power()

;; question 5

(define (fib n) ; tail 
  (define (h a b n)
    (if (= n 0)
        a
        (h b (+ a b) (- n 1))))
  (h 0 1 n))

(define (fib-tail n)
  (if (< n 0)
      "n must be a non-negative integer"
      (letrec [(h (lambda (a b n)
                         (if (= n 0)
                             a
                             (h b (+ a b) (- n 1)))))]
        (h 0 1 n))))

(check-equal? (fib-tail 10) 55)
(check-equal? (fib 10) 55)