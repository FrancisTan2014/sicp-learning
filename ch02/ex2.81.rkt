
#lang racket

(require "get-put.rkt"
         "complex-number-additive.rkt")

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

(define coercion-store (create-storage))
(define (put-coercion t1 t2 item) (coercion-store 'put t1 t2 item))
(define (get-coercion t1 t2) (coercion-store 'get t1 t2))

(define (apply-generic op . args)
    (let ([type-tags (map type-tag args)])
        (let ([proc (get op type-tags)])
            (if proc
                (apply proc (map contents args))
                (if (= (length args) 2)
                    (let ([type1 (car type-tags)]
                          [type2 (cadr type-tags)]
                          [a1 (car args)]
                          [a2 (cadr args)])
                        (if (equal? type1 type2)
                            (error
                              "No method for these types: APPLY-GENERIC"
                              (list op type-tags))
                            (let ([t1->t2 (get-coercion type1 type2)]
                                  [t2->t1 (get-coercion type2 type1)])
                                (cond (t1->t2
                                        (apply-generic op (t1->t2 a1) a2))
                                      (t2->t1
                                        (apply-generic op a1 (t2->t1 a2))
                                      (error
                                        "No method for these types: APPLY-GENERIC"
                                        (list op type-tags)))))))
                    (error
                      "No method for these types: APPLY-GENERIC"
                      (list op type-tags)))))))

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
    (put 'exp '(scheme-number scheme-number) 
        (lambda (x y) (tag (expt x y))))
    'done)

(define (install-arithmethic-packages)
    (install-rectangular-package)
    (install-polar-package)
    (install-complex-package)
    (install-rational-package)
    (install-scheme-number-package)
    'done)

(module+ main
    (install-arithmethic-packages)
    (define make-from-real-imag (get 'make-from-real-imag 'complex))
    (define make-from-mag-ang (get 'make-from-mag-ang 'complex))
    (define make-rat (get 'make-rat 'rational))
    (define make-scheme-number (get 'make 'scheme-number))

    (define (scheme-number->scheme-number n) n) 
    (define (complex->complex z) z) 
    (put-coercion 'scheme-number 'scheme-number scheme-number->scheme-number) 
    (put-coercion 'complex 'complex complex->complex)

    (define (exp x y) (apply-generic 'exp x y))
    ; got 27 as the answer
    (exp 3 3)
    ; this will cause dead-loop
    (exp (make-from-real-imag 1 1)
         (make-from-real-imag 1 1))
    )