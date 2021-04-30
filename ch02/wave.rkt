#lang racket

(require "ex2.23.rkt"
         "ex2.46.rkt"
         "ex2.47.rkt"
         "ex2.48.rkt"
         "sequence-operations.rkt")

(provide (all-defined-out))

(define (list-segments vectors)
    (define (iter rest result)
        (let ([curr (if (null? rest) '() (car rest))])
            (if (null? curr)
                result
                (let ([next (if (null? (cdr rest)) 
                                '() 
                                (car (cdr rest)))])
                    (if (null? next)
                        result
                        (iter (cdr rest) 
                              (cons (make-segment curr next) result)))))))
    (iter vectors '()))



(define wave-picture-points
    (list '(.25 .05) '(.35 .5) '(.3 .6) '(.15 .4) '(.05 .65)
          '(.05 .85) '(.15 .6) '(.3 .65) '(.4 .65) '(.35 .85)
          '(.4 .95) '(.6 .95) '(.65 .85) '(.6 .65) '(.75 .65)
          '(.95 .35) '(.95 .15) '(.6 .45) '(.75 .05) '(.6 .05)
          '(.5 .3) '(.4 .05) '(.25 .05)))

(define wave-picture-segments
    (list-segments 
        (accumulate 
            (lambda (curr-p seq) (cons (make-vect (car curr-p) (car (cdr curr-p)))
                                       seq))
            '()
            wave-picture-points)))

(module+ main
    (display wave-picture-points)
    (newline)
    (display wave-picture-segments))