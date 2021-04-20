#lang racket

(define tolerance .00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
           tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (cond ((close-enough? guess next) guess)
                  (else (display next) (newline) (try next)))))
    (try first-guess))

(fixed-point (lambda (x) (let ((log1000 (log 1000)))
                          (/ log1000 (log x))))
             2.0)