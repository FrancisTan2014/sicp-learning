#lang racket

(define (sum term a nekt b)
    (if (> a b)
        0
        (+ (term a)
           (sum term (nekt a) nekt b))))

(define (inc x) (+ x 1))
(define (cube x) (* x x x))

; the sample
(define (integral f a b dx)
    (define (add-dx x) (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)
       dx))

; integral by Simpson's Rule

(define (simpson-integral f a b n)
    (define h (/ (- b a) n))
    (define (coefficient-of k)
        (cond ((or (= k 0) (= k n)) 1)
              ((odd? k) 4)
              ((even? k) 2)))
    (define (y k) (f (+ a (* k h))))
    (define (simpson-term k)
        (* (coefficient-of k) (y k)))
    (/ (* h (sum simpson-term 0 inc n))
       3))
    
(integral cube 0 1 0.01) ; 0.24998750000000042
(simpson-integral cube 0 1 100.0) ; 0.24999999999999992
(integral cube 0 1 0.001) ; 0.249999875000001
(simpson-integral cube 0 1 1000.0) ; 0.2500000000000003

; obviously, the simpson-integral gives us a much better
; approximation to the integral while computing the same
; number of terms