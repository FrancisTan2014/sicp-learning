#lang racket

(require "ex2.23.rkt"
         "ex2.46.rkt"
         "ex2.47.rkt"
         "ex2.48.rkt")

(provide (all-defined-out))

; transform fomula: vect-new = Origin(Frame) + x*(Edge1(Frame)) + y*(Edge2(Frame))
(define (frame-coord-map frame)
    (lambda (v)
        (add-vect (origin-frame frame)
                  (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                            (scale-vect (ycor-vect v) (edge2-frame frame))))))

(module+ test
    (require rackunit)
    (let ([frame (make-frame (make-vect 0 1) (make-vect 3 4) (make-vect 7 8))]
          [v (make-vect 0 0)])
        (check-true (same-vect? ((frame-coord-map frame) v)
                                (origin-frame frame)))))

(define (segments->painter segment-list)
    (lambda (frame)
        (let ([transform (frame-coord-map frame)])
            (for-each (lambda (segment)
                        (draw-line (transform (start-segment segment))
                                   (transform (end-segment segment))))
                      segment-list))))