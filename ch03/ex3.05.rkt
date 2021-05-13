#lang racket

(define (estimate-integral predicate x1 x2 y1 y2 trials)
    (monte-carlo 
        trials
        (lambda ()
            (predicate (random-in-range x1 x2)
                       (random-in-range y1 y2)))))

(define (monte-carlo trials experiment)
    (define (iter remain-trials trials-passed)
        (cond ((= remain-trials 0)
               (/ trials-passed trials))
              ((experiment)
               (iter (- remain-trials 1)
                     (+ trials-passed 1)))
              (else
               (iter (- remain-trials 1)
                     trials-passed))))
    (iter trials 0))

(define (random-in-range low high)
    (let ([range (- high low)])
        (+ low (random range))))

(define (estimate-pi trials)
    (let ([radius 3]
          [center (make-point 5 7)]
          [corner1 (make-point 2 4)]
          [corner2 (make-point 8 10)])
        (/ (* (estimate-integral
                (lambda (x y)
                    (<= (square-sum center (make-point x y))
                        (sqr radius)))
                (x-coord corner1)
                (x-coord corner2)
                (y-coord corner1)
                (y-coord corner2)
                trials)
              (rect-area corner1 corner2))
           (sqr radius))))

(define (square-sum p1 p2)
    (+ (sqr (- (x-coord p1) (x-coord p2)))
       (sqr (- (y-coord p1) (y-coord p2)))))

(define (rect-area c1 c2)
    (* 1.0
       (abs (* (- (x-coord c1) (x-coord c2))
               (- (y-coord c1) (y-coord c2))))))

(define (make-point x y) (cons x y))
(define (x-coord p) (car p))
(define (y-coord p) (cdr p))

(module+ main
    (estimate-pi 100000))