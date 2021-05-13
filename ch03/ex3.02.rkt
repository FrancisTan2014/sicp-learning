#lang racket

(define (make-monitored procedure)
    (let ([count 0])
        (lambda (x)
            (cond ((eq? 'how-many-calls? x) count)
                  ((eq? 'reset-count x)
                    (begin (set! count 0) count))
                  (else 
                    (begin 
                        (set! count (+ count 1)) 
                        (procedure x)))))))

(module+ test
    (require rackunit)
    (let ([msqrt (make-monitored sqrt)])
        (check-eq? (msqrt 100) 10)
        (check-eq? (msqrt 'how-many-calls?) 1)
        (check-eq?
            (begin (msqrt 100) (msqrt 'how-many-calls?))
            2)
        (check-eq?
            (begin (msqrt 'reset-count) (msqrt 'how-many-calls?))
            0)))