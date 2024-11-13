#lang racket

(require "contracts.rkt")
(require rackunit)

(test-case "Make account with valid balance"
           (check-true (account-balance>=10? (make-account 100))
                       "Should create account with balance >= 10"))
  
(test-case "Fail to create account with insufficient balance"
           (check-exn #rx"Initial balance must be at least 10"
                      (lambda () (make-account 5))
                      "Should raise error for balance < 10"))
  
(test-case "Successful deposit"
           (let* ([initial-account (make-account 100)]
                  [after-deposit (deposit initial-account 50)])
             (check > (bank-account-balance after-deposit) 
                    (bank-account-balance initial-account)
                    "Deposit should increase balance")))
  
(test-case "Fail to deposit negative amount"
           (let ([initial-account (make-account 100)])
             (check-exn #rx"Invalid deposit"
                        (lambda () (deposit initial-account -10))
                        "Should raise error for negative deposit")))
  
(test-case "Successful withdrawal"
           (let* ([initial-account (make-account 100)]
                  [after-withdraw (withdraw initial-account 50)])
             (check < (bank-account-balance after-withdraw) 
                    (bank-account-balance initial-account)
                    "Withdrawal should decrease balance")))
  
(test-case "Fail to withdraw more than balance"
           (let ([initial-account (make-account 100)])
             (check-exn #rx"Invalid withdrawal"
                        (lambda () (withdraw initial-account 200))
                        "Should raise error for overdraw")))
  
(test-case "Fail to withdraw negative amount"
           (let ([initial-account (make-account 100)])
             (check-exn #rx"Invalid withdrawal"
                        (lambda () (withdraw initial-account -10))
                        "Should raise error for negative withdrawal")))
  
(test-case "Prevent balance from falling below 10"
           (let ([initial-account (make-account 20)])
             (check-exn #rx"Invalid withdrawal"
                        (lambda () (withdraw initial-account 11))
                        "Should prevent withdrawal that would reduce balance below 10")))