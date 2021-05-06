#lang racket

(require "set-tree.rkt")

(provide list->tree)

(define (list->tree elements)
    (car (partial-tree elements (length elements))))

(define (partial-tree items n)
    (if (= n 0)
        (cons '() items)
        (let ([left-size (quotient (- n 1) 2)])
            (let ([left-result (partial-tree items left-size)])
                (let ([left-tree (car left-result)])
                    (let ([right-size (- n (+ left-size 1))])
                        (let ([right-result (partial-tree (cddr left-result) right-size)])
                            (let ([right-tree (car right-result)]
                                  [entry (cadr left-result)])
                                (cons (make-tree
                                        entry
                                        left-tree
                                        right-tree)
                                      (cdr right-result))))))))))

(module+ main
    (list->tree '(1 3 5 7 9 11 13 15 17 19 21)))