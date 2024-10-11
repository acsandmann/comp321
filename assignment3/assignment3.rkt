#lang typed/racket

(require typed/rackunit)

;; 1)
; (b)
(define-struct point ([x : Real] [y : Real])
  #:transparent)

(: on-axis? (point -> Boolean))
(define (on-axis? p)
  (or (= (point-x p) 0) (= (point-y p) 0)))

(check-equal? (on-axis? (make-point 0 5)) #t)
(check-equal? (on-axis? (make-point 3 0)) #t)
(check-equal? (on-axis? (make-point 3 4)) #f)

; (c)
(: distance (point point Real -> Real))
(define (distance p1 p2 p)
  (cond
    [(>= p 1)
     (let ([dx (abs (- (point-x p1) (point-x p2)))]
           [dy (abs (- (point-y p1) (point-y p2)))])
       (real-part (expt (+ (expt dx p) (expt dy p)) (/ 1 p))))]
    [else (error 'distance "p must be >= 1")]))

(check-equal? (distance (point 3 4) (point 0 0) 2) 5)
(check-equal? (distance (point 3 4) (point 0 0) 1) 7)

; (d)
(: euclidean-dist (point point -> Real))
(define (euclidean-dist p1 p2)
  (distance p1 p2 2))

(: manhattan-dist (point point -> Real))
(define (manhattan-dist p1 p2)
  (distance p1 p2 1))

;; 3
(struct queue ([data : (Listof Any)]) #:transparent)

(: enqueue (Any queue -> queue)) ;; O(n)
(define (enqueue item q)
  (queue (append (queue-data q) (list item))))

(: dequeue (queue -> (U queue String))) ;; O(1)
(define (dequeue q)
  (if (null? (queue-data q)) "empty" (queue (rest (queue-data q)))))

(: peek (queue -> (U Any String)))
(define (peek q)
  (if (null? (queue-data q)) "empty" (first (queue-data q))))

(: empty (queue -> Boolean))
(define (empty q)
  (null? (queue-data q)))

; queue testing
(define q1 (queue '()))
(define q2 (enqueue 1 q1))
(define q3 (enqueue 2 q2))

(check-equal? (peek q3) 1)
(check-equal? (dequeue q3) (queue '(2)))
(check-equal? (empty q3) #f)

;; 4
; (a)
(: none? ((Any -> Boolean) (Listof Any) -> Boolean))
(define (none? pred lst)
  (cond
    [(empty? lst) #t]
    [(pred (first lst)) #f]
    [else (none? pred (rest lst))]))

; (b)
(: my-negate ((Any -> Boolean) -> (Any -> Boolean)))
(define (my-negate pred)
  (lambda (x) (not (pred x))))

; (c)
(: my-conjoin ((Any -> Boolean) (Any -> Boolean) -> (Any -> Boolean)))
(define (my-conjoin pred1 pred2)
  (lambda (x) (and (pred1 x) (pred2 x))))

; (d)
(: conjoin-all (All (A) (Listof (A -> Boolean)) -> (A -> Boolean)))
(define (conjoin-all plist)
  (lambda (x)
    (for/and ([pred plist])
      (pred x))))

; testing
(define f
  (conjoin-all (list (lambda ([n : Integer]) (> n 0))
                     (lambda ([n : Integer]) (<= n 10))
                     (lambda ([n : Integer]) (odd? n)))))

(check-equal? (map f (list 0 4 7)) (list #f #f #t))