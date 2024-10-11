#lang typed/racket

(: listof-str? (-> (Listof Any) Boolean))
(define (listof-str? lst)
  (andmap string? lst))


(: listof-num? (-> (Listof Any) Boolean))
(define (listof-num? lst)
  (andmap number? lst))


;; Define a stack element type. In this case, it holds Numbers, but you can change it.
(define-type StackElem Number)

;; Define an empty stack as a struct
(struct empty-stack ())

;; Define a non-empty stack as a struct with an element and the rest of the stack
(struct non-empty-stack ([elem : StackElem] [rest : Stack]))

;; Define a Stack as a union of empty and non-empty stack
(define-type Stack (U empty-stack non-empty-stack))

;; Stack operations

;; Check if the stack is empty
(: stack-empty? (Stack -> Boolean))
(define (stack-empty? stk)
  (empty-stack? stk))

;; Push a new element onto the stack
(: push (StackElem Stack -> Stack))
(define (push elem stk)
  (non-empty-stack elem stk))

;; Pop an element from the stack, returning the rest of the stack
(: pop (Stack -> (U Stack #f)))
(define (pop stk)
  (cond
    [(non-empty-stack? stk) (non-empty-stack-rest stk)]
    [else #f])) ;; Returns #f if trying to pop from an empty stack

;; Peek at the top element of the stack
(: peek (Stack -> (U StackElem #f)))
(define (peek stk)
  (cond
    [(non-empty-stack? stk) (non-empty-stack-elem stk)]
    [else #f])) ;; Returns #f if trying to peek at an empty stack
