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