#lang racket

(require "huffman-tree.rkt"
         "ex2.18.rkt")

(define (encode message tree)
    (if (null? message)
        '()
        (append (encode-symbol (car message) tree)
                (encode (cdr message) tree))))

(define (encode-symbol symbol tree)
    (define (iter branch result)
        (if (leaf? branch)
            result
            (let ([left-symbols (symbols (left-branch branch))]
                  [right-symbols (symbols (right-branch branch))])
                (cond ((include? symbol left-symbols)
                        (iter (left-branch branch) (cons 0 result)))
                      ((include? symbol right-symbols)
                        (iter (right-branch branch) (cons 1 result)))))))
    (reverse (iter tree '())))

(define (include? symbol list)
    (if (null? list)
        #f
        (if (eq? symbol (car list))
            #t
            (include? symbol (cdr list)))))

(module+ main
    (define sample-tree
        (make-code-tree 
            (make-leaf 'A 4)
            (make-code-tree
                (make-leaf 'B 2)
                (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
    (define sample-message '(A D A B B C A))
    (encode sample-message sample-tree))