#lang racket

(require 
    "get-put.rkt"
    "ex2.78.rkt")

(provide
    install-raise-package
    raise)

(define (install-raise-package)
    (define (scheme->rational x)
        ((get 'make-rat 'rational) x 1))
    (define (rational->complex x)
        ((get 'make-from-real-imag 'complex) x 0))
    (define (schema->complex x)
        (rational->complex (scheme->rational x)))

    (put 'raise 'scheme-number scheme->rational)
    (put 'raise 'complex schema->complex)
    (put 'raise 'rational rational->complex)
    'done)

(define (raise x) ((get 'raise (type-tag x)) x))

(module+ test
    (require rackunit)
    (install-arithmethic-packages)
    (install-raise-package)
    (define (rational? x) 
        (and (not (number? x))
             (eq? (type-tag x) 'rational)))
    (define (numer x) ((get 'numer 'rational) (contents x)))
    (define (denom x) ((get 'denom 'rational) (contents x)))
    (define (complex? x)
        (and (not (number? x))
             (eq? (type-tag x) 'complex)))

    (let ([r (raise 3)])
        (check-true (rational? r))
        (check-eq? (numer r) 3)
        (check-eq? (denom r) 1))
    )