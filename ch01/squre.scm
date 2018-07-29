(define (squre x)
    (* x x))
(squre 4)

;define sum of squres by squre
(define (sum-of-squres x y)
    (+ (squre x) (squre y)))
(sum-of-squres 3 4)