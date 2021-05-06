#lang racket

(require 
    "set-tree.rkt"
    "ex2.63.rkt"
    "ex2.64.rkt")

(define (union-set set1 set2)
    (define (iter tree result)
        (if (null? tree)
            result
            (let ([e-res (if (element-of-set? (entry tree) result)
                             result
                             (adjoin-set (entry tree) result))])
                (let ([r-res (iter (right-branch tree) e-res)])
                    (iter (left-branch tree) r-res)))))
    (list->tree 
        (tree-list 
            (iter set1 set2))))

(define (intersection-set set1 set2)
    (define (iter tree1 tree2 result)
        (if (null? tree1)
            result
            (let ([e-res (if (element-of-set? (entry tree1) tree2)
                             (cons (entry tree1) result)
                             result)])
                (let ([r-res (iter (right-branch tree1) tree2 e-res)])
                    (iter (left-branch tree1) tree2 r-res)))))
    (list->tree (iter set1 set2 '())))

(module+ main
    (define set1 (list->tree '(1 3 5 7 9 11)))
    (define set2 (list->tree '(3 7 9 13 15 17)))
    (union-set set1 set2)
    (intersection-set set1 set2))