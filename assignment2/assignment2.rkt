#lang racket
(require rackunit)

;; 1. 
(define (genFib)
  (let ([a 0] [b 1])
    (lambda ()
      (let ([next a])
        (set! a b)
        (set! b (+ next b))
        next))))

(define nextFib (genFib))
(check-equal? (nextFib) 0)
(check-equal? (nextFib) 1)
(check-equal? (nextFib) 1)
(check-equal? (nextFib) 2)
(check-equal? (nextFib) 3)

;; 3.
; a)
(define consonants '("b" "c" "d" "f" "g" "h" "j" "k" "l" "m" "n" "p" "q" "r" "s" "t" "v" "w" "x" "y" "z"))

(define (filter-consonants lst)
  (filter (lambda (char) (member char consonants)) lst))

(check-equal? (filter-consonants '("a" "b" "c" "e" "i" "o" "u" "d" "f")) '("b" "c" "d" "f"))

; b)
(define (sum-of-squares lst)
  (foldl (lambda (x acc) (+ (* x x) acc)) 0 lst))

(check-equal? (sum-of-squares '(1 2 3 4 5)) 55)

; c)
(define (flatten lst)
  (foldr (lambda (x acc)
           (append (if (list? x) (flatten x) (list x)) acc))
         '() lst))

(check-equal? (flatten '((1) 2 3 (4) ((5)) 6)) '(1 2 3 4 5 6))

; d)
(define (find-largest lst)
  (foldl max (car lst) lst))

(check-equal? (find-largest '(3 7 1 9 2 5)) 9)

; e)
(define (zip lst1 lst2)
  (cond
    [(or (empty? lst1) (empty? lst2)) '()]
    [else (cons (list (first lst1) (first lst2))
                (zip (rest lst1) (rest lst2)))]))

(check-equal? (zip '(1 2 3) '(4 5 6)) '((1 4) (2 5) (3 6)))

; f)
(define (filter-with-comparator cmp value lst)
  (filter (lambda (x) (cmp x value)) lst))

(check-equal? (filter-with-comparator <= 5 '(1 3 5 7 9)) '(1 3 5))

;; 4.
(define (merge lst1 lst2)
  (cond
    [(empty? lst1) lst2]
    [(empty? lst2) lst1]
    [(<= (first lst1) (first lst2))
     (cons (first lst1) (merge (rest lst1) lst2))]
    [else
     (cons (first lst2) (merge lst1 (rest lst2)))]))

(define (split lst)
  (let ([n (quotient (length lst) 2)])
    (list (take lst n) (drop lst n))))

(define (merge-sort lst)
  (if (<= (length lst) 1)
      lst
      (let* ([split-lists (split lst)]
             [left (merge-sort (car split-lists))]
             [right (merge-sort (cadr split-lists))])
        (merge left right))))

(check-equal? (merge-sort '(9 3 7 1 8 2 5 6 4)) '(1 2 3 4 5 6 7 8 9))

;; 5. (bonus)
(define (my-map f lst)
  (foldl (lambda (x acc) (cons (f x) acc)) '() (reverse lst)))

(check-equal? (my-map (lambda (x) (* x x)) '(1 2 3 4 5)) '(1 4 9 16 25))