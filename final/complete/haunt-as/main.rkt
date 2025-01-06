#lang racket/base

(require (for-syntax racket/base))
(require "helper.rkt")

(module reader racket/base
  (require syntax/strip-context
           racket/port)

  (provide read)
  (provide (rename-out [my-read-syntax read-syntax]))

  ;; Read-syntax implementation
  (define (my-read-syntax source-name in)
    (define src-stx
      (datum->syntax
       #f
       `(module haunt-as-module haunt-as
          ,@(let ([str (port->string in)])
              (with-input-from-string str
                                      (lambda ()
                                        (let loop ([forms '()])
                                          (let ([form (read-syntax source-name (current-input-port))])
                                            (if (eof-object? form)
                                                (reverse forms)
                                                (loop (cons (syntax->datum form) forms)))))))))
       #f))
    (strip-context src-stx)))

(provide (all-from-out racket/base)
         part1
         part2)

(define (part1 filename)
  (solve1 filename))

(define (part2 filename)
  (solve2 filename))
