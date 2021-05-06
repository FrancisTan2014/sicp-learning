#lang racket

(require "ex2.38.rkt"
         "../base/tools.rkt")

(define (reverse-l seq)
    (fold-left (lambda (first reversed) (append reversed (list first))) 
               '()
               seq))

(define (reverse-r seq)
    (fold-right (lambda (result first) (cons first result)) 
                '() 
                seq))

(module+ main
    (testing-methods (list reverse-r reverse-l) '(1 2 3 4)))