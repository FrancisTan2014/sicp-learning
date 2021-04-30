#lang racket

(require "ex2.46.rkt"
         "ex2.47.rkt"
         "ex2.48.rkt"
         "sequence-operations.rkt"
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

(define (list-segments . vectors)
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

(define (wave-painter frame) 
   ((segments->painter (list 
                        (make-segment (make-vect .25 0) (make-vect .35 .5)) 
                        (make-segment (make-vect .35 .5) (make-vect .3 .6)) 
                        (make-segment (make-vect .3 .6) (make-vect .15 .4)) 
                        (make-segment (make-vect .15 .4) (make-vect 0 .65)) 
                        (make-segment (make-vect 0 .65) (make-vect 0 .85)) 
                        (make-segment (make-vect 0 .85) (make-vect .15 .6)) 
                        (make-segment (make-vect .15 .6) (make-vect .3 .65)) 
                        (make-segment (make-vect .3 .65) (make-vect .4 .65)) 
                        (make-segment (make-vect .4 .65) (make-vect .35 .85)) 
                        (make-segment (make-vect .35 .85) (make-vect .4 1)) 
                        (make-segment (make-vect .4 1) (make-vect .6 1)) 
                        (make-segment (make-vect .6 1) (make-vect .65 .85)) 
                        (make-segment (make-vect .65 .85) (make-vect .6 .65)) 
                        (make-segment (make-vect .6 .65) (make-vect .75 .65)) 
                        (make-segment (make-vect .75 .65) (make-vect 1 .35)) 
                        (make-segment (make-vect 1 .35) (make-vect 1 .15)) 
                        (make-segment (make-vect 1 .15) (make-vect .6 .45)) 
                        (make-segment (make-vect .6 .45) (make-vect .75 0)) 
                        (make-segment (make-vect .75 0) (make-vect .6 0)) 
                        (make-segment (make-vect .6 0) (make-vect .5 .3)) 
                        (make-segment (make-vect .5 .3) (make-vect .4 0)) 
                        (make-segment (make-vect .4 0) (make-vect .25 0))))
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