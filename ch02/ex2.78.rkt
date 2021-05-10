
#lang racket

(require "../base/math.rkt"
         "get-put.rkt"
         "complex-number-additive.rkt")

(provide
    install-complex-package
    install-rational-package
    install-scheme-number-package
    attach-tag
    type-tag
    contents
    apply-generic
    install-arithmethic-packages)

(define (attach-tag type-tag contents)
    (if (number? contents)
        contents
        (cons type-tag contents)))
(define (type-tag z)
    (if (number? z)
        'scheme-number
        (car z)))
(define (contents z)
    (if (number? z)
        z
        (cdr z)))

(define (apply-generic op . args)
    (let ([type-tags (map type-tag args)])
        (let ([proc (get op type-tags)])
            (if proc
                (apply proc (map contents args))
                (error
                 "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

;; generic arithmetic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-complex-package)
    (define (real-part z) (apply-generic 'real-part z))
    (define (imag-part z) (apply-generic 'imag-part z))
    (define (magnitude z) (apply-generic 'magnitude z))
    (define (angel z) (apply-generic 'angel z))
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag '(rectangular)) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang '(polar)) r a))

    (define (add-complex z1 z2)
        (make-from-real-imag (+ (real-part z1) (real-part z2))
                             (+ (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (- (real-part z1) (real-part z2))
                             (- (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                           (+ (angel z1) (angel z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                           (- (angel z1) (angel z2))))

    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))

    'done)

(define (install-rational-package)
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

    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
    (put 'make-rat 'rational
        (lambda (n d) (tag (make-rat n d))))
    (put 'numer 'rational numer)
    (put 'denom 'rational denom)
    'done)

(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
        (lambda (x) (tag x)))
    'done)

(define (install-arithmethic-packages)
    (install-rectangular-package)
    (install-polar-package)
    (install-complex-package)
    (install-rational-package)
    (install-scheme-number-package)
    'done)

(module+ main
    (install-rectangular-package)
    (install-polar-package)
    (install-complex-package)
    (define make-from-real-imag (get 'make-from-real-imag 'complex))
    (define make-from-mag-ang (get 'make-from-mag-ang 'complex))

    (add (make-from-real-imag 3 4)
         (make-from-mag-ang 5 45))
    (mul (make-from-real-imag 1 2)
         (make-from-real-imag 3 4))
         
    (install-rational-package)
    (define make-rat (get 'make-rat 'rational))
    (add (make-rat 1 2)
         (make-rat 1 4))
         
    (install-scheme-number-package)
    (define make-scheme-number (get 'make 'scheme-number))
    (div (make-scheme-number 6)
         (make-scheme-number 3))
         
    (add 3 4))