#lang racket

(require "ex2.46.rkt"
         "ex2.47.rkt"
         "ex2.48.rkt"
         "wave.rkt"
         "picture-language.rkt")

(provide wave)

(define origin (make-vect 0.0 0.0))
(define right (make-vect 1.0 0.0))
(define top (make-vect 0.0 1.0))
(define top-right (make-vect 1.0 1.0))

(define (outline-painter frame)
    ((segments->painter (list (make-segment origin right)
                              (make-segment right top-right)
                              (make-segment top-right top)
                              (make-segment top origin)))
     frame))

(define (diamond-painter frame)
    (let ([o (mid-segment (make-segment origin right))]
          [r (mid-segment (make-segment right top-right))]
          [t (mid-segment (make-segment top origin))]
          [tr (mid-segment (make-segment top-right top))])
        ((segments->painter (list (make-segment o r)
                                  (make-segment r tr)
                                  (make-segment tr t)
                                  (make-segment t o)))
         frame)))

(define (x-painter frame)
    ((segments->painter (list (make-segment origin top-right)
                              (make-segment right top)))
     frame))

(define (wave-painter frame) 
   ((segments->painter wave-picture-segments)
    frame))

(module+ main
    (let ([frame (make-frame (make-vect 1 1)
                             (make-vect 0 100)
                             (make-vect 100 0))])
        (outline-painter frame)
        (diamond-painter frame)
        (x-painter frame))
    (let ([frame (make-frame (make-vect 1 599)
                             (make-vect 798 0)
                             (make-vect 0 -599))])
        (wave-painter frame)))