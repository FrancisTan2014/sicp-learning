#lang racket

; first way is defined in picture-language.rkt

(require 
    "ex2.50.rkt"
    "wave.rkt"
    "picture-language.rkt")

(define (below-with-beside top-painter bottom-painter)
    (flip-horiz (beside (flip-horiz top-painter 270)
                        (flip-horiz bottom-painter 270))
     90))

(module+ main
    ((below-with-beside wave wave) window))