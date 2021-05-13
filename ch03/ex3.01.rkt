#lang racket

(define (make-accumulator init-value)
    (let ([value init-value])
        (lambda (n)
            (begin
                (set! value (+ value n))
                value))))

(module+ test
    (require rackunit)
    (let ([a1 (make-accumulator 0)])
        (check-eq? (a1 5) 5)
        (let ([a2 (make-accumulator 0)])
            (a2 10)
            (check-eq? (a1 0) 5))))