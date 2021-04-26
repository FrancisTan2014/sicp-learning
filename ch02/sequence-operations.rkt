#lang racket

(require "../base/math.rkt"
         "../ch01/fib-iter.rkt")

(provide filter
         accumulate
         enumerate-tree
         enumerate-interval)

(define (sum-odd-squares tree)
    (cond ((null? tree) 0)
          ((not (pair? tree))
            (if (odd? tree) (square tree) 0))
          (else (+ (sum-odd-squares (car tree))
                   (sum-odd-squares (cdr tree))))))

(define (even-fibs n)
    (define (next k)
        (if (> k n)
            '()
            (let ((f (fib k)))
                (if (even? f)
                    (cons f (next (+ k 1)))
                    (next (+ k 1))))))
    (next 0))

(define (filter predicate sequence)
    (cond ((null? sequence) '())
          ((predicate (car sequence))
            (cons (car sequence)
                  (filter predicate (cdr sequence))))
          (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
    (if (null? sequence)
        initial
        (op (car sequence)
            (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
    (if (> low high)
        '()
        (cons low 
              (enumerate-interval (+ low 1)
                                  high))))

(define (enumerate-tree tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree))
                        (enumerate-tree (cdr tree))))))

(define (sum-odd-squares-h tree)
    (accumulate +
                0
                (filter odd? (map square (enumerate-tree tree)))))

(define (even-fibs-h n)
    (accumulate cons
                '()
                (filter even? (map fib (enumerate-interval 0 n)))))

(module+ main
    (filter even? (list 1 2 3 4 5 6 7))
    (accumulate + 0 '(1 2 3 4 5))
    (accumulate * 1 '(1 2 3 4 5))
    (accumulate cons '(6) '(1 2 3 4 5))
    (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))