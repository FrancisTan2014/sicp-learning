#lang racket

(require 
    "ex2.46.rkt"
    "wave.rkt"
    "picture-language.rkt")

(provide (all-defined-out))

(define (flip-horiz painter degree)
    (let ([a (make-vect 0 0)]
            [b (make-vect 0 1)]
            [c (make-vect 1 1)]
            [d (make-vect 1 0)])
        (cond ((or (= degree 0)
                   (= degree 360))
                painter)
                ((= degree 90) (transform-painter
                                painter b a c))
                ((= degree 180) (transform-painter
                                painter c b d))
                ((= degree 270) (transform-painter
                                painter d c a)))))

(module+ main
    ((below (beside (flip-horiz wave 0) (flip-horiz wave 90))
            (beside (flip-horiz wave 180) (flip-horiz wave 270)))
     window))