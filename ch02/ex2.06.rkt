#lang racket

(define zero (lambda (f) (lambda (x) x)))
(define (add-1 n)
    (lambda (f)
        (lambda (x) (f ((n f) x)))))
    
; (add-1 zero)
(define one (lambda (f)
                (lambda (x) (f x))))

; (add-1 one)
; (n f) ->
; (lambda (x) (f x))
(define two (lambda (f)
                (lambda (x) (f (f x)))))

(module+ test
    (require rackunit)
    (let ((f (lambda (x) (* x x))))
        (check-eq? (((add-1 zero) f) 3) 
                   ((one f) 3))
        (check-eq? (((add-1 one) f) 3)
                   ((two f) 3))))

; define +
; make (+ one one) equals two
; (one f) ->
;  f1 = (lambda (x) (f x))
; (one f1) ->
; (lambda (x) ((lambda (x) (f x)) x))
(define (+ a b)
    (lambda (f) 
        (lambda (x) ((a f) ((b f) x)))))

(module+ test
    (let ((f (lambda (x) (* x x))))
        (check-eq? (((+ zero one) f) 3) 
                   ((one f) 3))
        (check-eq? (((+ one one) f) 3)
                   ((two f) 3))
        (check-eq? (((+ two one) f) 3)
                   (((add-1 two) f) 3))))