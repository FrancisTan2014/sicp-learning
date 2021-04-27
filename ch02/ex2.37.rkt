#lang racket

(require "ex2.36.rkt")

(define (dot-product v w)
    (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
    (map (lambda (vi) (dot-product vi v))
         m))

(define (transpose m)
    (accumulate-n cons '() m))

; (define (matrix-*-matrix m n) ??)
; this one need to learn much about
; the liner algebra, so I gave up it

(define (print-vector v)
    (cond ((null? v) (newline))
          (else (display (car v))
                (display " ")
                (print-vector (cdr v)))))
(define (print-matrix m)
    (map print-vector m))

(module+ main
    (dot-product '(1 2 3 4) '(5 6 7 8))
    (newline)
    (let ((m (list '(1 2 3 4) '(2 4 6 6) '(6 7 8 9)))
          (v '(2 2 2 2)))
        (print-matrix m)
        (newline)
        (print-vector (matrix-*-vector m v))
        (newline)
        (print-matrix (transpose m))
        (newline)))