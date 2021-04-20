#lang racket

(define (product-r term a next b)
    (if (> a b)
        1
        (* (term a)
           (product-r term (next a) next b))))

(define (product-i term a next b)
    (define (iter n result)
        (if (> n b)
            result
            (iter (next n)
                  (* (term n) result))))
    (iter a 1))

(define (identity x) x)
(define (inc x) (+ x 1))

(define (v n) (display n))

(define (square x) (* x x))
(define (odd x) (+ (* 2 x) 1))
(define (even x) (* 2 x))

(define (compute-pi n product)
    (define (nume-term x) 
        (cond ((= x 0) 1)
              ((= x 1) 2)
              ((= x n) (even x))
              (else (square (even x)))))
    (define (deno-term x)
        (cond ((or (= x 0) (= x n)) 1)
            (else (square (odd x)))))
    (* (/ (product nume-term 0 inc n)
          (product deno-term 0 inc n))
       4.0))

(define (measure p)
    (define start (current-inexact-milliseconds))
    (define result (p))
    (define end (current-inexact-milliseconds))
    (fprintf (current-output-port)
             "result: ~s ellapsed: ~sms\n"
             result
             (- end start)))

(define size 40000)
(measure (lambda () (compute-pi size product-r)))
(measure (lambda () (compute-pi size product-i)))