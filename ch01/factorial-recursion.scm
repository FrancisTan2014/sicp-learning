(define (fib n)
    (if (= n 1)
        n
        (* n (fib (- n 1)))))