#lang racket

(define tolerance .00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
        (< (abs (- v1 v2))
           tolerance))
    (define (try guess)
        (let ((next (f guess)))
            (if (close-enough? guess next)
                guess
                (try next))))
    (try first-guess))

(define (phi)
    (fixed-point (lambda (x) (+ 1 (/ 1 x)))
                 1.0))

(define (cont-frac-r n d k)
    (define (iter i)
        (if (= i k)
            (/ (n i) (d i))
            (/ (n i)
               (+ (d i)
                  (iter (+ i 1))))))
    (iter 1))

; iterative version 1
(define (cont-frac-i n d k)
    ; iterate from the bottom to the top
    ; after each step, we make the fomula
    ; simpler by eliminate the expression
    ; of the denominator
    (define (accurate-denom i denom)
        (cond ((= i 1) denom)
              (else (accurate-denom (- i 1)
                                    (+ (d (- i 1))
                                       (/ (n i)
                                          denom))))))
    (/ (n 1)
       (accurate-denom k (d k))))

; iterative version 2
(define (cont-frac-i-2 n d k)
    (define (iter i result)
        (if (= i 0)
            result
            (iter (- i 1)
                  (/ (n i)
                     (+ (d i) result)))))
    (iter k 0))

(define (power x y)
    (define (iter i result)
        (if (> i y)
            result
            (iter (+ i 1)
                  (* x result))))
    (iter 1 1))

; test if y is an approximation of x that 
; is accurate to d decimal places
(define (appro x y d)
    (< (abs (* (- x y) (power 10 d)))
       1.0))

(define cont-frac cont-frac-i-2)
(define phi-1 (/ 1 (phi)))
(define (cont-frac-1 k) 
    (cont-frac (lambda (x) 1.0)
               (lambda (x) 1.0)
               k))
(define (find-k d)
    (define (try k)
        (if (appro phi-1
                   (cont-frac-1 k)
                   (+ d 1)) ; cause (- 0.61801 0.61791) cannot be presented by the float number precisely
            k
            (try (+ k 1))))
    (try 1))

(fprintf (current-output-port) "1/phi: ~s\n" phi-1)
(define decimal-places 4)
(define k (find-k decimal-places))
(fprintf (current-output-port)
         "for k=~s we got cont-frac-1=~s\nwhich is accurate to ~s decimal places"
         k
         (cont-frac-1 k)
         decimal-places)
