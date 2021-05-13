#lang racket

(define (make-account balance password)
    (let ([balance balance]
          [password password]
          [incorrect-times 0])
        (lambda (pwd op)
            (lambda (amount)
                (if (< incorrect-times 7)
                    (if (equal? pwd password)
                        (cond ((eq? op 'withdraw)
                                (if (< amount balance)
                                    (begin 
                                        (set! balance (- balance amount))
                                        balance)
                                    "Insufficient funds"))
                              ((eq? op 'deposit)
                                (begin
                                    (set! balance (+ balance amount))
                                    balance))
                              (else (error "Unknown request: MAKE-ACCOUNT" op)))
                        (begin
                            (set! incorrect-times (+ incorrect-times 1))
                            "Incorrect password"))
                    "Call the cops!!!")))))

(module+ test
    (require rackunit)
    (let ([password 'testing])
        (define (repeat action times)
            (if (= times 1)
                (action)
                (begin
                    (action)
                    (repeat action (- times 1)))))
        (let ([acc (make-account 100 password)])
            (check-eq?
                (begin (repeat
                            (lambda () ((acc 'wrong-pwd 'withdraw) 40))
                            7)
                       ((acc password 'withdraw) 40))
                "Call the cops!!!"))))