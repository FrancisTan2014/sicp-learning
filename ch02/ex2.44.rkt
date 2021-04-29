#lang racket

(define (up-right painter n)
    (if (= n 0)
        painter
        (let ([smaller (up-right painter (- n 1))])
            (below painter (beside smaller smaller))))) 