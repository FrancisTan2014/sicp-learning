#lang racket

(require "sequence-operations.rkt")

(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (sum-list l) (accumulate + 0 l))

(define (unique-triples n)
    (flatmap (lambda (i)
                (flatmap (lambda (j) 
                            (map (lambda (k) (list i j k))
                                (enumerate-interval 1 (- j 1))))
                        (enumerate-interval 1 (- i 1))))
             (enumerate-interval 1 n)))

(define (find-triples n s)
    (filter (lambda (t) (= (sum-list t) s))
            (unique-triples n)))

(module+ main
    (sum-list '(3 2 1))
    (unique-triples 5)
    (find-triples 32 17))