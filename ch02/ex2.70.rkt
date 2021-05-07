#lang racket

(require "huffman-tree.rkt"
         "ex2.68.rkt"
         "ex2.69.rkt")

(module+ main
    (define pairs
        '((A 2) (GET 2) (SHA 3) (WAH 1)
          (BOOM 1) (JOB 2) (NA 16) (YIP 9)))
    (define huffman-tree (generate-huffman-tree pairs))
    (define lyrics '(GET A JOB SHA NA NA NA NA NA NA NA NA
                     GET A JOB SHA NA NA NA NA NA NA NA NA
                     WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP
                     SHA BOOM))
    (define bits '(1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 1 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1 1 0 1 0))
    (define encoded-bits (encode lyrics huffman-tree))
    (define decoded-chars (decode bits huffman-tree))
    encoded-bits
    (length encoded-bits)
    decoded-chars)