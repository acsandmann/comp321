#lang racket/base

(require racket/match
         racket/list
         racket/string
         racket/port)

(provide solve1
         solve2)

(define (parse-node-line line)
  (let* ([parts (string-split line " = ")]
         [key (first parts)]
         [values-str (second parts)]
         [values (string-split (string-replace (string-replace values-str "(" "") ")" "") ", ")])
    (cons key (cons (first values) (second values)))))

(define (parse-input input-str)
  (let* ([lines (string-split input-str "\n\n")]
         [directions (string->list (first lines))]
         [node-lines (filter (lambda (l) (not (string=? l ""))) (string-split (second lines) "\n"))])
    (cons directions (make-immutable-hash (map parse-node-line node-lines)))))

(define (next-node nodes current dir)
  (let ([node-pair (hash-ref nodes current)])
    (if (eq? dir #\L)
        (car node-pair)
        (cdr node-pair))))

(define (follow-path-to-zzz nodes directions current [step 0])
  (if (equal? current "ZZZ")
      step
      (follow-path-to-zzz
       nodes
       directions
       (next-node nodes current (list-ref directions (modulo step (length directions))))
       (add1 step))))

(define (ends-with-char? str char)
  (equal? (string-ref str (sub1 (string-length str))) char))

(define (find-starting-nodes nodes)
  (filter (lambda (key) (ends-with-char? key #\A)) (hash-keys nodes)))

(define (steps-to-z nodes directions current [step 0])
  (if (ends-with-char? current #\Z)
      step
      (steps-to-z nodes
                  directions
                  (next-node nodes current (list-ref directions (modulo step (length directions))))
                  (add1 step))))

(define (gcd a b)
  (if (zero? b)
      a
      (gcd b (modulo a b))))

(define (lcm-list nums)
  (foldl (lambda (a b) (/ (* a b) (gcd a b))) (car nums) (cdr nums)))

(define (solve1 filename)
  (define input-str (with-input-from-file filename (lambda () (port->string (current-input-port)))))
  (let ([parsed (parse-input input-str)]) (follow-path-to-zzz (cdr parsed) (car parsed) "AAA")))

(define (solve2 filename)
  (define input-str (with-input-from-file filename (lambda () (port->string (current-input-port)))))
  (let* ([parsed (parse-input input-str)]
         [nodes (cdr parsed)]
         [directions (car parsed)]
         [starting-nodes (find-starting-nodes nodes)])
    (lcm-list (map (lambda (start) (steps-to-z nodes directions start)) starting-nodes))))
