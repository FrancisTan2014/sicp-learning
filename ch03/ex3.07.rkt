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

(define (make-joint account password new-password)
    (if (number? ((account password 'withdraw) 0))
        (lambda (pwd op)
            (if (eq? pwd new-password)
                (account password op)
                "Incorrect password"))
        (error
            "Wrong password for the specified account: MAKE-JOINT"
            (list account password new-password))))

(module+ test
    (require rackunit)
    (let ([peter-pwd 'peter-pwd]
          [paul-pwd 'paul-pwd])
        (let ([peter-acc (make-account 100 peter-pwd)])
            (let ([paul-acc (make-joint peter-acc peter-pwd paul-pwd)])
                (check-eq? 
                    ((peter-acc 'wrong-pwd 'withdraw) 40)
                    "Incorrect password")
                (check-eq?
                    (paul-acc peter-pwd 'withdraw)
                    "Incorrect password")
                (check-eq?
                    ((peter-acc peter-pwd 'withdraw) 30)
                    70)
                (check-eq?
                    ((paul-acc paul-pwd 'withdraw) 10)
                    60)
                (check-eq?
                    ((peter-acc peter-pwd 'withdraw) 70)
                    "Insufficient funds")
                (check-eq? 
                    ((paul-acc paul-pwd 'deposit) 40)
                    100)))))