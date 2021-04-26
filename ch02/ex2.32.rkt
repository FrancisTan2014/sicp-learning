#lang racket

(define (subsets s)
    (if (null? s)
        (list '())
        (let ((rest (subsets (cdr s))))
            (append rest (map (lambda (x) (cons (car s) x))
                              rest)))))

(module+ main
    (subsets '(1 2 3)))

; ??? how to explain this in words
; It's a typical divide-and-conquer method.
; The subsets of set s contains two parts: 
; subsets which do not include the first
; element of s and subsets which include 
; the first element. The algrithm is like:
;   - if s is null, () is the only one subset
;   - otherwise, subsets of s contains:
;       - subsets of the cdr of s
;       - subsets that are created by adding the 
;         first element of s to each one of the previous subsets