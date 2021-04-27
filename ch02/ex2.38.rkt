#lang racket

(define (fold-left op initial seq)
    (if (null? seq)
        initial
        (op (car seq)
            (fold-left op initial (cdr seq)))))

(define (fold-right op initial seq)
    (define (iter rest result)
        (if (null? rest)
            result
            (iter (cdr rest)
                  (op result (car rest)))))
    (iter seq initial))

(module+ main
    (fold-left + 0 '(1 2 3 4 5))
    (fold-right + 0 '(1 2 3 4 5))
    (fold-left / 1 '(1 2 3))
    (fold-right / 1 '(1 2 3))
    (fold-left list '() '(1 2 3))
    (fold-right list '() '(1 2 3)))

; to make fold-left and fold-right produce
; the same results, op must satisy: 
;   (op x y) = (op y x)
; e.g.: + * or and