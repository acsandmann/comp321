#lang racket

; a. Define bank-account struct with contract
(provide (struct-out bank-account))
(struct bank-account (balance) #:transparent)

; Contract for bank-account ensuring balance is at least 10
(provide account-balance>=10?)
(define (account-balance>=10? acc)
  (and (bank-account? acc)
       (>= (bank-account-balance acc) 10)))

; b & c. Define functions with contracts
(provide make-account balance withdraw deposit)

(define (make-account initial-balance)
  (unless (>= initial-balance 10)
    (error 'make-account "Initial balance must be at least 10"))
  (bank-account initial-balance))

(define (balance account)
  (unless (account-balance>=10? account)
    (error 'balance "Invalid account"))
  (bank-account-balance account))

(define (withdraw account amount)
  (let ([current-balance (bank-account-balance account)])
    (unless (and (>= current-balance 10)  ; current balance is at least 10
                 (>= current-balance amount)  ; can withdraw the amount
                 (>= amount 0)  ; non-negative withdrawal
                 (>= (- current-balance amount) 10))  ; balance won't fall below 10
      (error 'withdraw "Invalid withdrawal"))
    (bank-account (- current-balance amount))))

(define (deposit account amount)
  (unless (and (account-balance>=10? account)
               (>= amount 0))
    (error 'deposit "Invalid deposit"))
  (bank-account (+ (bank-account-balance account) amount)))