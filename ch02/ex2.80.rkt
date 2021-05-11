#lang racket

(require 
    "get-put.rkt"
    "ex2.78.rkt")

(provide 
    install-zero-package
    =zero?)

(define (install-zero-package)
    (define (=zero-ordinary? x) (= x 0))
    (define (=zero-rational? x)
        (let ([numer (get 'numer 'rational)])
            (= (numer x) 0)))
    (define (=zero-complex? x)
        (let ([real-part (lambda (z) (apply-generic 'real-part z))]
              [imag-part (lambda (z) (apply-generic 'imag-part z))])
            (and (= (real-part x) 0)
                 (= (imag-part x) 0))))

    (put '=zero? '(scheme-number) =zero-ordinary?)
    (put '=zero? '(rational) =zero-rational?)
    (put '=zero? '(complex) =zero-complex?)
    'done)

(define (=zero? x) (apply-generic '=zero? x))

(module+ test
    (require rackunit)
    (install-arithmethic-packages)
    (install-zero-package)

    (check-true (=zero? 0))
    (check-false (=zero? 3))

    (define (make-rat n d) ((get 'make-rat 'rational) n d))
    (check-true (=zero? (make-rat 0 1)))
    (check-false (=zero? (make-rat 3 4)))
    
    (define (make-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
    (check-true (=zero? (make-from-real-imag 0 0)))
    (check-false (=zero? (make-from-real-imag 3 4)))
    )
