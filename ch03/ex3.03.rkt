#lang racket

(define (make-account balance password)
    (let ([balance balance]
          [password password])
        (lambda (pwd op)
            (lambda (amount)
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
                    "Incorrect password")))))

(module+ test
    (require rackunit)
    (let ([password 'testing])
        (let ([acc (make-account 100 password)])
            (check-eq? 
                ((acc 'wrong-pwd 'withdraw) 40)
                "Incorrect password")
            (check-eq?
                ((acc password 'withdraw) 40)
                60)
            (check-eq?
                ((acc password 'withdraw) 70)
                "Insufficient funds")
            (check-eq? 
                ((acc password 'deposit) 40)
                100))))