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
    (list '(.25 .01) '(.35 .5) '(.3 .6) '(.15 .4) '(.01 .65)
          '(.01 .85) '(.15 .6) '(.3 .65) '(.4 .65) '(.35 .85)
          '(.4 .99) '(.6 .99) '(.65 .85) '(.6 .65) '(.75 .65)
          '(.99 .35) '(.99 .15) '(.6 .45) '(.75 .01) '(.6 .01)
          '(.5 .3) '(.4 .01) '(.25 .01)))

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