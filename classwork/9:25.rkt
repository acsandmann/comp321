#lang racket
(require rackunit)

(+ 5 (+ 6 7))

;; |-x: T1 |-e:T2 (lambda (x:T1) E): T1 -> T2

(bytes->string/utf-8 #"\316\273")

;; latin-1 has no lambda character
; (string->bytes/latin-1 "λ")

(define (norm-factorial n)
  (if (= n 1) 1
      (* n (norm-factorial (- n 1))
         ))
  )

(define factorial-table (make-hash))

(define (hash-factorial n)
  (let ((result (hash-ref factorial-table n #f)))
    (if result
        result
        (let ((computed-result
               (if (= n 1)
                   1
                   (* n (hash-factorial (- n 1))))))
          (hash-set! factorial-table n computed-result)
          computed-result))))

(check-equal? (norm-factorial 10) 3628800)
(check-equal? (hash-factorial 10) 3628800)