#lang racket

(require 
    "ex2.85.rkt"
    "get-put.rkt")

(define (install-polynomial-package)
    (define (make-poly variable terms) (cons variable terms))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (same-variable? p1 p2)
        (eq? (variable p1) (variable p2)))
    (define (empty-term-list? l) (null? l))
    (define (the-empty-term-list) '())
    (define (ajoin-term term term-list)
        (if (=zero? (coff term))
            term-list
            (cons term term-list)))
    (define (first-term terms) (car terms))
    (define (rest-terms terms) (cdr terms))
    (define (make-term order coff) (list order coff))
    (define (order t) (car t))
    (define (coff t) (cadr t))

    (define (add-poly p1 p2)
        (if (same-variable? p1 p2)
            (make-poly (variable p1)
                       (add-terms (term-list p1) (term-list p2)))
            (error 
                "Polys not in same var: ADD-POLY"
                (list p1 p2))))

    (define (mul-poly p1 p2)
        (if (same-variable? p1 p2)
            (make-poly (variable p1)
                       (mul-terms (term-list p1) (term-list p2)))
            (error 
                "Polys not in same var: MUL-POLY"
                (list p1 p2))))

    (define (add-terms l1 l2)
        (cond ((empty-term-list? l1) l2)
              ((empty-term-list? l2) l1)
              (else
                (let ([t1 (first-term l1)]
                      [t2 (first-term l2)])
                    (cond ((> (order t1) (order t2))
                           (ajoin-term
                             t1
                             (add-terms (rest-terms l1) l2)))
                          ((< (order t1) (order t2))
                           (ajoin-term
                             t2
                             (add-terms l1 (rest-terms l2))))
                          (else
                           (ajoin-term
                             (make-term (order t1)
                                        (add (coff t1) (coff t2)))
                             (add-terms (rest-terms l1) (rest-terms l2)))))))))

    (define (mul-terms l1 l2)
        (if (empty-term-list? l1)
            (the-empty-term-list)
            (add-terms
                (mul-term-by-all-terms (first-term l1) l2)
                (mul-terms (rest-terms l1) l2))))

    (define (mul-term-by-all-terms t1 l)
        (if (empty-term-list? l)
            (the-empty-term-list)
            (let ([t2 (first-term l)])
                (ajoin-term
                    (make-term (add (order t1) (order t2))
                               (mul (coff t1) (coff t2)))
                    (mul-term-by-all-terms t1 (rest-terms l))))))
    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) 
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put '=zero? '(polynomial) 
        (lambda (p) (empty-term-list? (term-list p))))
    (put 'make 'polynomial
        (lambda (var terms) (tag (make-poly var terms))))
    'done)

(define (make-poly var terms)
    ((get 'make 'polynomial) var terms))

(module+ main
    (install-arithmethic-packages)
    (install-polynomial-package)
    (define p1 (make-poly 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))
    (define p2 (make-poly 'x '((100 1) (2 2) (0 1))))
    (add p1 p2)
    (mul p1 p2)
    )