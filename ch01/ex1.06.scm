(load "abs3.scm")
(define (new-if predicate then-clause else-clause)
    (cond (predicate then-clause)
          (else else-clause)
    )
)
(define (square x) (* x x))
(define (average x y)
    (/ (+ x y) 2))
(define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.0000000001))
(define (improve guess x)
    (average guess (/ x guess)))
(define (sqrt-iter guess x)
    (new-if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
(define (sqrt x)
    (sqrt-iter 1.0 x))

; Oooops! We got ';Aborting!: maximum recursion depth exceeded' 
; when call sqrt to get the square root. The reason is:
; https://stackoverflow.com/questions/1171252/whats-the-explanation-for-exercise-1-6-in-sicp