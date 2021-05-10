#lang racket

(require "../base/math.rkt")

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
(define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
(define (make-from-mag-ang r a) (cons r a))
(define (real-part z) (* (magnitude z) (cos (angel z))))
(define (imag-part z) (* (magnitude z) (sin (angel z))))
(define (magnitude z) (car z))
(define (angel z) (cdr z))

(module+ main
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