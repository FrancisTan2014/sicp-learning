#lang racket

(require "../base/math.rkt"
         "get-put.rkt")

(provide
    install-rectangular-package
    install-polar-package)

(define (attach-tag type-tag contents) 
    (cons type-tag contents))
(define (type-tag z) (car z))
(define (contents z) (cdr z))

(define (install-rectangular-package)
    ;; internal procedures
    (define (make-from-real-imag x y) (cons x y))
    (define (make-from-mag-ang r a) 
        (cons (* r (cos a))
              (* r (sin a))))
    (define (real-part z) (car z))
    (define (imag-part z) (cdr z))
    (define (magnitude z) 
        (sqrt (+ (square (real-part z)) 
                 (square (imag-part z)))))
    (define (angel z) (atan (imag-part z) (real-part z)))
    
    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'rectangular x))
    (put 'make-from-real-imag '(rectangular)
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(rectangular)
        (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'real-part '(rectangular) real-part)
    (put 'imag-part '(rectangular) imag-part)
    (put 'magnitude '(rectangular) magnitude)
    (put 'angel '(rectangular) angel)
    'done)

(define (install-polar-package)
    ;; internal procedures
    (define (make-from-real-imag x y)
        (cons (sqrt (+ (square x) (square y)))
              (atan y x)))
    (define (make-from-mag-ang r a) (cons r a))
    (define (real-part z) (* (magnitude z) (cos (angel z))))
    (define (imag-part z) (* (magnitude z) (sin (angel z))))
    (define (magnitude z) (car z))
    (define (angel z) (cdr z))
    
    ;; interface to the rest of the system
    (define (tag x) (attach-tag 'polar x))
    (put 'make-from-real-imag '(polar)
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang '(polar)
        (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'real-part '(polar) real-part)
    (put 'imag-part '(polar) imag-part)
    (put 'magnitude '(polar) magnitude)
    (put 'angel '(polar) angel)
    'done)

(define (apply-generic op . args)
    (let ([type-tags (map type-tag args)])
        (let ([proc (get op type-tags)])
            (if proc
                (apply proc (map contents args))
                (error
                 "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

(module+ test
    (require rackunit)
    (put 'add '(arith) (lambda (pair) (+ (car pair) (cdr pair))))
    (check-eq? (apply-generic 'add (attach-tag 'arith (cons 3 4))) 7))

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

(module+ main
    (install-rectangular-package)
    (install-polar-package)
    (define sum     
        (add-complex
            (make-from-real-imag 3 4)
            (make-from-real-imag 5 6)))
    (define diff
        (sub-complex
            (make-from-mag-ang 5 45)
            (make-from-real-imag 3 4)))
    (display "sum: ")
    (display (real-part sum)) 
    (display " ") 
    (display (imag-part sum)) 
    (newline)
    (display "diff: ")
    (display (real-part diff)) 
    (display " ")
    (display (imag-part diff)))