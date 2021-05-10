#lang racket

(require 
    "get-put.rkt"
    "ex2.78.rkt")

(provide install-equal-package)

(define (install-equal-package)
    (define (equ-ordinary? x y) (= x y))
    (define (equ-rational? x y)
        (let ([numer (get 'numer 'rational)]
              [denom (get 'denom 'rational)])
            (and (= (numer x) (numer y))
                 (= (denom x) (denom y)))))
    (define (equ-complex? x y)
        (let ([real-part (lambda (z) (apply-generic 'real-part z))]
              [imag-part (lambda (z) (apply-generic 'imag-part z))])
            (and (= (real-part x) (real-part y))
                 (= (imag-part x) (imag-part y)))))

    (put 'equ? '(scheme-number scheme-number) equ-ordinary?)
    (put 'equ? '(rational rational) equ-rational?)
    (put 'equ? '(complex complex) equ-complex?)
    'done)

(module+ main
    (install-arithmethic-packages)
    (install-equal-package)

    (define (equ? x y) (apply-generic 'equ? x y))
    (equ? 3 3)

    (define (make-rat n d) ((get 'make-rat 'rational) n d))
    (equ? (make-rat 3 4) (make-rat 2 3))
    
    (define (make-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
    (equ? (make-from-real-imag 3 4) (make-from-real-imag 3 4))
    (equ? (make-from-real-imag 3 4) (make-from-real-imag 3 3))
    )
