#lang racket

(define (create-f)
    (let ([value #f]
          [visit-times 0])
        (lambda (v)
            (set! visit-times (+ visit-times 1))
            (if (> visit-times 1)
                0
                (if value
                    value
                    (begin
                        (set! value v)
                        value))))))

(module+ test
    (require rackunit)
    (let ([f (create-f)])
        (check-eq? (+ (f 0) (f 1)) 0))
    (let ([f (create-f)])
        (check-eq? (+ (f 1) (f 0)) 1)))