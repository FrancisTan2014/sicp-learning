#lang racket

(require "set-tree.rkt"
         "../base/tools.rkt")

(provide tree-list)

(define (tree->list-1 tree)
    (if (null? tree)
        '()
        (append (tree->list-1 (left-branch tree))
                (cons (entry tree)
                      (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
    (define (copy-to-list tree result)
        (if (null? tree)
            result
            (copy-to-list 
                (left-branch tree)
                (cons (entry tree)
                      (copy-to-list
                        (right-branch tree)
                        result)))))
    (copy-to-list tree '()))

(define tree-list tree->list-1)

(module+ main
    (define tree1
        (make-tree 7
            (make-tree 3 
                (make-tree 1 '() '())
                (make-tree 5 '() '()))
            (make-tree 9
                '()
                (make-tree 11 '() '()))))
    (define tree2
        (make-tree 3
            (make-tree 1 '() '())
            (make-tree 7
                (make-tree 5 '() '())
                (make-tree 9
                    '()
                    (make-tree 11 '() '())))))
    (define tree3
        (make-tree 5
            (make-tree 3
                (make-tree 1 '() '())
                '())
            (make-tree 9
                (make-tree 7 '() '())
                (make-tree 11 '() '()))))
    (define tree4
        (adjoin-set 11
            (adjoin-set 9
                (adjoin-set 7
                    (adjoin-set 5
                        (adjoin-set 3
                            (adjoin-set 1 '())))))))
    (define tree5
        (make-tree 11
            (make-tree 9
                (make-tree 7
                    (make-tree 5
                        (make-tree 3
                            (make-tree 1 '() '())
                            '())
                        '())
                    '())
                '())
            '()))
    (tree->list-1 tree1)
    (tree->list-2 tree1)
    (newline) 
    (tree->list-1 tree2)
    (tree->list-2 tree2)
    (newline)
    (tree->list-1 tree3)
    (tree->list-2 tree3)
    (newline)
    (tree->list-1 tree4)
    (tree->list-2 tree4)
    (newline)
    (tree->list-1 tree5)
    (tree->list-2 tree5)
    (newline))