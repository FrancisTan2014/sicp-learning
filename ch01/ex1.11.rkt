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

(f-r 0) (f-i 0)
(f-r 1) (f-i 1)
(f-r 2) (f-i 2)
(f-r 3) (f-i 3)
(f-r 4) (f-i 4)
(f-r 5) (f-i 5)
(f-r 6) (f-i 6)
(f-r 7) (f-i 7)
(f-r 8) (f-i 8)
(f-r 9) (f-i 9)
(f-r 10) (f-i 10)
(f-r 20) (f-i 20)
(f-r 30) (f-i 30)
(f-r 40) (f-i 40)
