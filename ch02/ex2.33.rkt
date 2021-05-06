#lang racket

(require 
    "../base/math.rkt"
    "sequence-operations.rkt")

(define (map p sequence)
    (accumulate (lambda (x y) (cons (p x) y))
                '()
                sequence))

(define (append seq1 seq2)
    (accumulate cons '() (enumerate-tree (list seq1 seq2))))

(define (length sequence)
    (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

(module+ main
    (map square '(1 2 3 4))
    (append '(1 2 3 4) '(5 6 7 8))
    (length '(1 2 3 4)))