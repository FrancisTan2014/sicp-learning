#lang racket

(require "../base/math.rkt"
         "ex1.42.rkt")

(provide repeated)

(define (repeated f n)
    (define (iter i result)
        (if (= i n)
            result
            (iter (+ i 1)
                  (compose f result))))
    (iter 1 f))


(module+ main
    ((repeated square 2) 5))