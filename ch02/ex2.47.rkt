#lang racket

(provide make-frame
         origin-frame
         edge1-frame
         edge2-frame)

(define (make-frame origin edge1 edge2)
    (list origin edge1 edge2))
(define (origin-frame frame) (car frame))
(define (edge1-frame frame) (caar frame))
(define (edge2-frame frame) (caaar frame))

(define (make-frame-2 origin edge1 edge2)
    (cons origin (cons edge1 edge2)))
(define (origin-frame-2 frame) (car frame))
(define (edge1-frame-2 frame) (car (cdr frame)))
(define (edge2-frame-2 frame) (cdr (cdr frame)))

