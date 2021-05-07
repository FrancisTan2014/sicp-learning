#lang racket

(require "huffman-tree.rkt"
         "ex2.68.rkt")

; generate a huffman tree with given pairs
(define (generate-huffman-tree pairs)
    (successive-merge (make-leaf-set pairs)))

(define (successive-merge leaf-set)
    (if (null? leaf-set)
        '()
        (let ([first (car leaf-set)]
              [second (if (null? (cdr leaf-set))
                          '()
                          (cadr leaf-set))])
            (if (null? second)
                first
                (successive-merge
                    (adjoin-set
                        (make-code-tree first second)
                        (cddr leaf-set)))))))

(module+ main
    (define huffman-tree (generate-huffman-tree '((A 4) (B 2) (C 1) (D 1))))
    (decode '(0 1 1 0 0 1 0 1 0 1 1 1 0) huffman-tree)
    (encode '(A D A B B C A) huffman-tree))