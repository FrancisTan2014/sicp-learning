#lang racket

(require 
    "get-put.rkt"
    "ex2.78.rkt"
    "ex2.79.rkt")

(provide
    install-raise-package
    raise
    can-raise?)

(define (install-raise-package)
    (define (scheme->rational x)
        ((get 'make-rat 'rational) x 1))
    (define (rational->complex x)
        ((get 'make-from-real-imag 'complex) x 0))

    (put 'raise 'scheme-number scheme->rational)
    (put 'raise 'rational rational->complex)
    'done)

(define (raise x)
    (let ([m (get 'raise (type-tag x))])
        (if m
            (m x)
            (error 
              "no method for these types: RAISE" 
              (list 'raise (type-tag x))))))

(define (can-raise? x)
    (let ([m (get 'raise (type-tag x))])
        (if m #t #f)))

(module+ test
    (require rackunit)
    (install-arithmethic-packages)
    (install-equal-package)
    (install-raise-package)
    (define (rational? x) 
        (and (not (number? x))
             (eq? (type-tag x) 'rational)))
    (define (numer x) ((get 'numer 'rational) (contents x)))
    (define (denom x) ((get 'denom 'rational) (contents x)))
    (define (complex? x)
        (and (not (number? x))
             (eq? (type-tag x) 'complex)))
    (define (real-part z)
        ((get 'real-part 'complex) (contents z)))
    (define (imag-part z)
        ((get 'imag-part 'complex) (contents z)))

    (let ([r (raise 3)])
        (check-true (rational? r))
        (check-eq? (numer r) 3)
        (check-eq? (denom r) 1)
        (let ([c (raise r)])
            (check-true (complex? c))
            (check-true (equ? (real-part c) r))
            (check-eq? (imag-part c) 0)
            (check-false (can-raise? c)))))