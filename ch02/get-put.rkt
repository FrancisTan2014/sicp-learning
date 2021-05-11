#lang racket

(provide 
    create-storage
    storage
    put
    get)

(define (make-box op type item)
    (list op type item))
(define (get-op box) (car box))
(define (get-type box) (cadr box))
(define (get-item box) (caddr box))
(define (match? op type box)
    (and (equal? (get-op box) op)
         (equal? (get-type box) type)))

(module+ test
    (require rackunit)
    (let ([box (make-box 'test 'test 3)])
        (check-true (match? 'test 'test box))
        (check-eq? (get-item box) 3))
    (let ([box (make-box 'test '(test) 3)])
        (check-true (match? 'test '(test) box))))

(define (create-storage)
    (let ([table '()])
        (define (put op type item)
            (set! table (cons (make-box op type item)
                              table)))
        (define (get op type)
            (define (find list)
                (if (null? list)
                    #f
                    (let ([first (car list)])
                        (if (match? op type first)
                            (get-item first)
                            (find (cdr list))))))
            (find table))
        (define (list-all) table)
        (lambda (m . args)
            (cond ((eq? m 'put) (apply put args))
                  ((eq? m 'get) (apply get args))
                  ((eq? m 'list) (apply list-all args))))))

(define storage (create-storage))
(define (put op type item) (storage 'put op type item))
(define (get op type) (storage 'get op type))