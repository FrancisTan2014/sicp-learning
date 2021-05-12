#lang racket

(require 
    "ex2.85.rkt"
    "get-put.rkt")

(define (neg x) (apply-generic 'neg x))

(define (install-negation-package)
    (put 'neg '(scheme-number) (lambda (x) (- x)))
    (put 'neg '(rational)
        (lambda (r)
            ((get 'make-rat 'rational)
                (- ((get 'numer 'rational) r))
                ((get 'denom 'rational) r))))
    (put 'neg '(complex)
        (lambda (c)
            ((get 'make-from-real-imag 'complex)
                (neg ((get 'real-part 'complex) c))
                (neg ((get 'imag-part 'complex) c)))))
    'done)

(define (install-polynomial-package)
    (define (make-poly variable terms) (cons variable terms))
    (define (variable p) (car p))
    (define (term-list p) (cdr p))
    (define (same-variable? p1 p2)
        (eq? (variable p1) (variable p2)))
    (define (empty-term-list? l) (null? l))
    (define (the-empty-term-list) '())
    (define (ajoin-term term term-list)
        (if (=zero? (coeff term))
            term-list
            (cons term term-list)))
    (define (first-term terms) (car terms))
    (define (rest-terms terms) (cdr terms))
    (define (make-term order coeff) (list order coeff))
    (define (order t) (car t))
    (define (coeff t) (cadr t))
    (define (=zero-poly? p) (empty-term-list? (term-list p)))
    (define (neg-poly p) 
        (if (=zero-poly? p)
            p
            (make-poly
                (variable p)
                (map
                    (lambda (t) (make-term 
                                    (order t) 
                                    (neg (coeff t))))
                    (term-list p)))))

    (define (add-poly p1 p2)
        (if (same-variable? p1 p2)
            (make-poly (variable p1)
                       (add-terms (term-list p1) (term-list p2)))
            (error 
                "Polys not in same var: ADD-POLY"
                (list p1 p2))))

    (define (sub-poly p1 p2)
        (if (same-variable? p1 p2)
            (add-poly p1 (neg-poly p2))
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
                                        (add (coeff t1) (coeff t2)))
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
                               (mul (coeff t1) (coeff t2)))
                    (mul-term-by-all-terms t1 (rest-terms l))))))

    (define (tag p) (attach-tag 'polynomial p))
    (put 'add '(polynomial polynomial) 
        (lambda (p1 p2) (tag (add-poly p1 p2))))
    (put 'sub '(polynomial polynomial)
        (lambda (p1 p2) (tag (sub-poly p1 p2))))
    (put 'mul '(polynomial polynomial)
        (lambda (p1 p2) (tag (mul-poly p1 p2))))
    (put '=zero? '(polynomial) =zero-poly?)
    (put 'make 'polynomial
        (lambda (var terms) (tag (make-poly var terms))))
    (put 'neg '(polynomial)
        (lambda (p) (tag (neg-poly p))))
    'done)

(define (make-poly var terms)
    ((get 'make 'polynomial) var terms))

(module+ main
    (install-arithmethic-packages)
    (install-negation-package)
    (install-polynomial-package)

    (define p5 
        (make-poly 'x
                   (list (list 2 (make-poly 'y '((1 1) (0 1))))
                         (list 1 (make-poly 'y '((2 1) (0 1))))
                         (list 0 (make-poly 'y '((1 1) (0 -1)))))))
    (define p6
        (make-poly 'x
                   (list (list 1 (make-poly 'y '((1 1) (0 -2)))
                         (list 0 (make-poly 'y '((3 1) (0 7))))))))

    (add p5 p6)
    (sub p5 p6)
    (mul p5 p6)

    (define p1 (make-poly 'x '((5 1) (4 2) (2 3) (1 -2) (0 -5))))
    (define p2 (make-poly 'x '((100 1) (2 2) (0 1))))
    (add p1 p2)
    (sub p1 p2)
    (mul p1 p2)

    (define make-rat (get 'make-rat 'rational))
    (define make-complex (get 'make-from-real-imag 'complex))
    (define p3 (make-poly 'x (list '(2 3) (list 1 (make-complex 2 3)) '(0 7))))
    (define p4 (make-poly 'x (list '(4 1) (list 2 (make-rat 2 3)) (list 0 (make-complex 5 3)))))
    (neg p4)
    (add p3 p4)
    (sub p3 p4)
    )