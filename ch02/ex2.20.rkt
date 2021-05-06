; aims to practise the usage of list in Lisp
; and introduces a new built-in feature of Lisp
;   -> arbitrary numbers of arguments

#lang racket

(require "ex2.18.rkt")

; strategy: iterative
; key: same parity algrithm

(define (same? a b)
    (= (remainder a 2)
       (remainder b 2)))
(define (same-parity n . seq)
    (define (iter l result)
        (if (null? l)
            result
            (iter (cdr l)
                  (if (same? (car l) n)
                      (cons (car l) result)
                      result))))
    (reverse (iter seq (list n))))

; testing
(module+ main
    (same-parity 1 2 3 4 5 6 7)
    (same-parity 2 3 4 5 6 7)
    (same-parity 1 2 4 6 8))