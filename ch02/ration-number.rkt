#lang racket

(define (cons x y)
    (lambda (i) (if (= i 1) x y)))

(define (car p) (p 1))
(define (cdr p) (p 2))

(define (make-rat n d) 
    (let ((g (gcd n d)))
        (cons (/ n g) (/ d g))))
(define (numer r) (car r))
(define (denom r) (cdr r))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
            
(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))

(define (eq-rat? x y)
    (= (* (numer x) (denom y))
       (* (denom x) (numer y))))

(define (print-rat r)
    (newline)
    (display (numer r))
    (display "/")
    (display (denom r)))

(module+ test
    (require rackunit)
    (check-true (eq-rat? (add-rat (make-rat 3 4) 
                                  (make-rat 2 3)) 
                         (make-rat 17 12))))

(module+ main
    (define one-half (make-rat 1 2))
    (print-rat one-half)
    (define one-third (make-rat 1 3))
    (print-rat (add-rat one-half one-third))
    (print-rat (mul-rat one-half one-third))
    (print-rat (add-rat one-third one-third)))