(define (min x y)
    (if (< x y)
        x
        y))
(define (sum-of-big-nums x y z)
    (- (+ x y z)
       (min (min x y) (min y z))))
(sum-of-big-nums 3 8 2)