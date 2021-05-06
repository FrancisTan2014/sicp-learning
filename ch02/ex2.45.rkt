#lang racket

; practising of capturing patterns 
; to define high-order procedures

(define (split first-part-aligh second-part-aligh)
    (lambda (painter n)
        (let ([smaller (((spilt first-part-aligh second-part-aligh) painter (- n 1)))])
            (first-part-aligh painter (second-part-aligh smaller smaller)))))
    
(define right-split (spilt beside below))
(define up-right (spilt below beside))