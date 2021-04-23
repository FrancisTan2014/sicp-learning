#lang racket

; top level
(define (cons a b)
    (define (compute p base)
        (let ((other (- 5 base))) ; Hint: 5=2+3
            (define (iter result) ; e.g.: 108=2^2*3*3
                (if (mod? result other) ; keep dividing 3 until the remainder of result%3 is not 0 
                   (iter (/ result other))
                   (log result base))) ; then 4 is the left number, and a must be (log 4 2)
            (iter p)))
    (let ((p (product (power 2 a)
                      (power 3 b))))
        (lambda (x) (cond ((= x 2) (compute p 2))
                          ((= x 3) (compute p 3))))))

(define (car p) (p 2))
(define (cdr p) (p 3))

(module+ test
    (require rackunit)
    (let ((p (cons 2 3)))
        (check-eq? (car p) 2)
        (check-eq? (cdr p) 3))
    (let ((p (cons 1 1)))
        (check-eq? (car p) 1)
        (check-eq? (cdr p) 1))
    (let ((p (cons 0 0)))
        (check-eq? (car p) 0)
        (check-eq? (cdr p) 0)))

; abstract barriers

; abstract barriers
(define (product x y) (* x y))
(define (power x y)
    (define (iter i result)
        (if (> i y)
            result
            (iter (+ i 1) (* x result))))
    (iter 1 1))
(define (mod? a b)
    (= (remainder a b)
       0))
(define (log n base)
    (define (iter count num)
        (if (= num 1)
            count
            (iter (+ count 1) (/ num base))))
    (iter 0 n))

(module+ test
    (check-eq? (power 2 3) 8)
    (check-eq? (power 3 3) 27)
    (check-true (mod? 8 2))
    (check-false (mod? 8 3))
    (check-eq? (log 8 2) 3)
    (check-eq? (log 27 3) 3))