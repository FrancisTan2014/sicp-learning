#lang racket

(provide (all-defined-out))

; top layer
(define (midpoint-segment line)
    (midpoint (start-point line)
              (end-point line)))

; segment layer, we need:
; make-segment, start-point, end-point
(define (make-segment start end) (cons start end))
(define (start-point line) (car line))
(define (end-point line) (cdr line))
(define (midpoint a b)
    (make-point (average (x-coor a) (x-coor b))
                (average (y-coor a) (y-coor b))))

; point layer, we need:
; make-point, x-coor, y-coor
(define (make-point x y) (cons x y))
(define (x-coor p) (car p))
(define (y-coor p) (cdr p))
(define (print-point p) 
    (fprintf
        (current-output-port)
        "(~s,~s)\n" 
        (x-coor p) 
        (y-coor p)))
             
; base
(define (average x y) (/ (+ x y) 2.0))

(module+ main
    (print-point (midpoint-segment (make-segment (make-point 1 4)
                                                  (make-point 2 5)))))