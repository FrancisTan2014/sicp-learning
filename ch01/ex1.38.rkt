#lang racket

(define (cont-frac n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1)
                  (/ (n i)
                     (+ (d i) result)))))
    (iter k 0))

(define (n i) 1.0)
(define (d i)
    (let ((rem (remainder i 3)))
          (if (or (= rem 0) (= rem 1))
              1
              (* 2 (+ 1 (quotient i 3))))))

(define (iter a b)
    (cond ((> a b) (display (d a)))
          (else (display (d a))
                (iter (+ a 1) b))))

(define (approximate-e k)
    (+ 2
       (cont-frac n d k)))

(approximate-e 1)
(approximate-e 2)
(approximate-e 3)
(approximate-e 4)
(approximate-e 5)
(approximate-e 6)
(approximate-e 7)
(approximate-e 8)
(approximate-e 9)
(approximate-e 10)
(approximate-e 11)
(approximate-e 1100)