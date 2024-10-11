#lang plait
(require "interpreter.rkt")

(test (calc (num 1)) 1)
(test (calc (num 2.3)) 2.3)
(test (calc (plus (num 1) (num 2))) 3)
(test (calc (plus (num 0.10) (num 0.23))) 1/3)

(test (run `2) 2)
(test (run `(+ 1 2)) 3)