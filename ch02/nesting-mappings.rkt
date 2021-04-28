#lang racket

(require "sequence-operations.rkt")

(provide (all-defined-out))

(define (flatmap proc seq)
    (accumulate append '() (map proc seq)))

(define (prime? n)
    (accumulate (lambda (x y) (and (not (= (remainder n x) 0))
                                   y))
                #t 
                (enumerate-interval 2 (- n 1))))

(define (pair-sum pair)
    (+ (car pair)
       (cdr pair)))

(define (prime-sum? pair)
    (prime? (pair-sum pair)))

(define (make-pair-sum pair)
    (list (car pair) (cdr pair) (pair-sum pair)))

(define (prime-sum-pairs n)
    (map make-pair-sum
         (filter prime-sum?
                 (flatmap (lambda (i)
                            (map (lambda (j) (list i j))
                                 (enumerate-interval 1 (- i 1))))
                          (enumerate-interval 1 n)))))

(define (permutations s)
    (if (null? s)
        (list '())
        (flatmap (lambda (x)
                    (map (lambda (p) (cons x p))
                         (permutations (remove x s))))
                 s)))

(define (remove item sequence)
    (filter (lambda (x) (not (= x item)))
            sequence))

(module+ main
    (prime? 1)
    (prime? 2)
    (prime? 3)
    (pair-sum '(4 1))
    (prime-sum? '(4 1))
    (permutations '(1 2 3)))