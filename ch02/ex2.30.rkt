#lang racket

(require "../base/math.rkt")

(provide (all-defined-out))

(define (square-tree-d tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (square tree))
          (else (cons (square-tree-d (car tree))
                      (square-tree-d (cdr tree))))))

(define (square-tree-m tree)
    (map (lambda (sub-tree)
            (if (pair? sub-tree)
                (square-tree-m sub-tree)
                (square sub-tree)))
         tree))

(module+ main
    (define tree (list 1 (list 2 (list 3 4) 5) (list 6 7)))
    (define methods (list square-tree-d square-tree-m))
    (map (lambda (m) (m tree)) methods))