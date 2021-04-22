#lang racket

(require "../base/math.rkt"
         "../base/unit.rkt"
         "fixed-point.rkt"
         "ex1.43.rkt")

(define (average-damp-n n)
    (λ (f) ((repeated average-damp n) f)))
(define (sqrt-4 x)
    (fixed-point ((average-damp-n 2) (λ (y) (/ x (cube y))))
                 1.0))

(module+ test
    (check-in-tolerance (sqrt-4 16) 2.0)
    (check-in-tolerance (sqrt-4 81) 3.0))

(define (root-of x power damp-builder [first-guess 1.0])
    (define f (λ (y) (/ x (expt y (- power 1)))))
    (fixed-point (damp-builder f)
                 first-guess))

(module+ main
    (root-of 1024 4 (average-damp-n 2))
    (root-of 1024 8 (average-damp-n 3))
    (root-of 1024 16 (average-damp-n 4))
    (root-of 1024 32 (average-damp-n 5))
    (root-of 1024 64 (average-damp-n 6))
    (root-of 1024 128 (average-damp-n 7))
    (root-of 1024 256 (average-damp-n 8))
    (root-of 1024 512 (average-damp-n 9))
    (root-of 1024 1024 (average-damp-n 10)))

; for nth root, need log2n times of average-damp
(define (nth-root x n)
    (root-of x n (average-damp-n (floor (log n 2)))))

(module+ main
    (newline)
    (nth-root 1024 10)
    (nth-root 1024 16)
    (nth-root 1024 32)
    (nth-root 1024 35)
    (nth-root 1024 64))