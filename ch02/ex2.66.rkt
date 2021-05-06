#lang racket

(require 
    "set-tree.rkt"
    "ex2.63.rkt"
    "ex2.64.rkt")

(define (lookup key tree)
    (cond ((null? tree) '())
          ((= key (entry tree)) (entry tree))
          ((< key (entry tree)) (lookup key (left-branch tree)))
          ((> key (entry tree)) (lookup key (right-branch tree)))))

(module+ main
    (define tree (list->tree '(1 2 3 4 5 6 7 8 9 10 11)))
    (lookup 0 tree)
    (lookup 12 tree)
    (lookup 10 tree))