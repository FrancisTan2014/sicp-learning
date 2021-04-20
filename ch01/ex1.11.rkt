#lang racket

; 函数 f 由如下规则定义：
;     如果 n < 3，那么 f(n) = n;
;     如果 n >= 3，那么 f(n) = f(n-1) + 2f(n-2) + 3f(n-3)。
; 请写一个采用递归计算过程计算 f 的过程。
; 再写一个采用迭代计算过程计算 f 的过程。

; recursive
(define (f-r n)
    (if (< n 3)
        n
        (+ (f-r (- n 1))
           (* 2 (f-r (- n 2)))
           (* 3 (f-r (- n 3))))))


; iterative
; f(n) = f(n-1) + 2f(n-2) + 3f(n-3)
;   for a is f(n-1)
;       b is f(n-2)
;       c is f(n-3)
; when n -> n + 1
; then a' = a + 2b + 3c
;      b' = a
;      c' = b

(define (f-i n)
    (define (f-iter a b c num)
        (if (> num n)
            a
            (if (< num 3)
                (f-iter 
                    num
                    (- num 1)
                    (- num 2)
                    (+ num 1))
                (f-iter (+ a (* 2 b) (* 3 c))
                        a
                        b
                        (+ num 1)))))
    (f-iter 0 0 0 0))
