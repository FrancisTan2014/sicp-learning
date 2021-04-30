#lang racket

(define (memq item x)
    (cond ((null? x) #f)
          ((eq? item (car x)) x)
          (else (memq item (cdr x)))))

(module+ test
    (require rackunit)
    (check-false (memq 'apple '(pear banana prune)))
    (check-eq? (memq 'apple '(x (apple sauce) y apple pear))
               '(apple pear)))

(module+ main
    (list 'a 'b 'c) ; a b c right is: '(a b c)
    (list (list 'george)) ; (('george)) right is: '((george))
    (cdr '((x1 x2) (y1 y2))) ; '(y1 y2) right is: '((y1 y2))
    (cadr '((x1 x2) (y1 y2))) ; '(y1 y2)
    (pair? (car '(a short list))) ; #f
    (memq 'red '((red shoes) (blue socks))) ; #f
    (memq 'red '(red shoes blue socks)) ; '(red shoes blue socks)
    )