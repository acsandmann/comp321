#lang racket

(require rackunit)

(define-syntax-rule (assert expr)
  (unless expr
    (error 'assert "~a did not evaluate to true; actual result: ~a" 'expr expr)))


;;(assert (= 2 (+ 1 1))) ; This will pass, as 2 = 1 + 1 is true
;;(assert (= 2 (+ 1 2))) ; This will raise an error: 'assert: (= 2 (+ 1 2)) did not evaluate to true'

(define-syntax-rule (noisy-eval expr)
  (let ([result expr])
    (printf "evaluating ~a\n" 'expr)
    (printf "result: ~a\n" result)
    (printf "done\n")
    result))

(noisy-eval (+ 1 2)) ; Prints "evaluating (+ 1 2)", evaluates (+ 1 2), then prints "done".

(println "------------------")

(define-syntax for-loop
  (syntax-rules ()
    [(for-loop ([i e0 limit]) body)
     (for-loop ([i e0 limit 1]) body)]
    [(for-loop ([i e0 limit stride]) body)
     (let loop ([i e0])
       (when (< i limit)
         body
         (loop (+ i stride))))]))

(for-loop ([i 0 5]) (printf "~a\n" i))
; Output: 0 1 2 3 4
(println "------------------")
(for-loop ([i 0 10 2]) (printf "~a\n" i))
; Output: 0 2 4 6 8
(println "------------------")

#|(define-syntax (my-case stx)
  (syntax-case stx (else)
    [(my-case val) #'(void)]
    [(my-case val [else result-expr]) #'(result-expr)]
    [(my-case val [(datum ...) result-expr] clause ...)
     #'(if (member val '(datum ...))
           result-expr
           (my-case val clause ...))]))|#

(define-syntax (my-case stx)
  (syntax-case stx ()
    [(my-case val clause ...)
     #'(let ([temp val])
         (my-case-helper temp clause ...))]))


(define-syntax (my-case-helper stx)
  (syntax-case stx ()
    [(my-case-helper val)
     #'(void)]
    
    [(my-case-helper val [else result-expr])
     #'(result-expr)]
    
    [(my-case-helper val [(datum ...) result-expr] clause ...)
     #'(if (member val '(datum ...))
           result-expr
           (my-case-helper val clause ...))]))

(check-equal? (my-case 3
  [(1 2) "one or two"]
  [(3 4) "three or four"]
  [else "default"]) "three or four")
