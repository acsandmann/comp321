#lang haunt-as

(define solution1 (part1 "input2.txt"))
(define solution2 (part2 "input2.txt"))

(displayln (format "Part 1 Solution: ~a" solution1))
(displayln (format "Part 2 Solution: ~a" solution2))

(require rackunit)
  (check-true (or (equal? solution1 17263) (equal? solution1 12361)))
  (check-true (or (equal? solution2 14631604759649) (equal? solution2 18215611419223)))

