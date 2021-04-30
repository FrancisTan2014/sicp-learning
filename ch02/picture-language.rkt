#lang racket

(require graphics/graphics)

(require "ex2.23.rkt"
         "ex2.46.rkt"
         "ex2.47.rkt"
         "ex2.48.rkt"
         "wave.rkt"
         "racket-graphics.rkt")

(provide (all-defined-out))

(define window-width 1200)
(define window-height 800)

(if (graphics-open?) 'opened (open-graphics))
(define vp (open-viewport "main" window-width window-height))
(define (vect-2-posn v) (make-posn (xcor-vect v) (ycor-vect v)))
(define (draw-line v1 v2)
    ((draw-line-r vp) (vect-2-posn v1)
             (vect-2-posn v2)
             (make-rgb 0 0 0)))

(define window
    (make-frame
        (make-vect 0 window-height)
        (make-vect window-width 0)
        (make-vect 0 (- window-height))))

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

(define (wave frame)
    ((segments->painter wave-picture-segments) frame))

(define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
        (let ([m (frame-coord-map frame)])
            (let ([new-origin (m origin)])
                (painter (make-frame
                            new-origin
                            (sub-vect (m corner1) new-origin)
                            (sub-vect (m corner2) new-origin)))))))

(define (flip-vert painter)
    (transform-painter 
        painter 
        (make-vect 1 1)
        (make-vect 0 1)
        (make-vect 1 0)))

(define (shrink-to-upper-right painter)
    (transform-painter
        painter
        (make-vect 0.5 0.5)
        (make-vect 1.0 0.5)
        (make-vect 0.5 1.0)))

(define (rotate90 painter)
    (transform-painter
        painter
        (make-vect 1 0)
        (make-vect 1 1)
        (make-vect 0 0)))

(define (squash-inwards painter)
    (transform-painter
        painter
        (make-vect 0 0)
        (make-vect 0.65 0.35)
        (make-vect 0.35 0.65)))

(define (beside painter-left painter-right)
    (let ([split-point (make-vect 0.5 0)])
        (let ([paint-left (transform-painter
                            painter-left
                            (make-vect 0 0)
                            split-point
                            (make-vect 0 1))]
                [paint-right (transform-painter
                            painter-right
                            split-point
                            (make-vect 1 0)
                            (make-vect 0.5 1))])
            (lambda (frame)
                (paint-left frame)
                (paint-right frame)))))

(define (below painter-top painter-bottom)
    (let ([split-point (make-vect 0 0.5)])
        (let ([paint-top (transform-painter
                            painter-top
                            split-point
                            (make-vect 1 0.5)
                            (make-vect 0 1))]
              [paint-bottom (transform-painter
                            painter-bottom
                            (make-vect 0 0)
                            (make-vect 1 0)
                            split-point)])
            (lambda (frame)
                (paint-top frame)
                (paint-bottom frame)))))

(module+ main
    ; (wave window)
    ; ((flip-vert wave) window)
    ; ((shrink-to-upper-right wave) window)
    ; ((rotate90 wave) window)
    ; ((squash-inwards wave) window)
    ((beside wave wave) window)
    ; ((below wave wave) window)
    )