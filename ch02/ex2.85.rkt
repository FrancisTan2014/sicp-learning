#lang racket

(require "get-put.rkt"
         "complex-number-additive.rkt")

(provide
    attach-tag
    type-tag
    contents
    apply-generic
    equ?
    =zero?
    install-arithmethic-packages
    add
    sub
    mul
    div)

(define (install-project-package)
    (define (project-complex c)
        ((get 'real-part 'complex) (contents c)))
    (define (project-rational r)
        ((get 'numer 'rational) (contents r)))

    (put 'project 'complex project-complex)
    (put 'project 'rational project-rational)
    'done)

(define (drop n)
    (define (project start)
        (let ([tag (type-tag start)])
            (if tag
                (let ([sub-type (sub-type-of (type-tag start))])
                    (if sub-type
                        (let ([lower ((get 'project (type-tag start)) start)])
                            (let ([raise-back (car (make-same (list lower start)))])
                                (if (equ? raise-back start)
                                    (project lower)
                                    start)))
                        start))
                start)))
    (project n))

(define (sub-type-of type)
    (define (iter type rest)
        (if (null? rest)
            #f
            (let ([first (car rest)])
                (if (same-type? first type)
                    (if (null? (cdr rest))
                        #f
                        (cadr rest))
                    (iter type (cdr rest))))))
    (iter type (arith-tower 'list)))

(define (attach-tag type-tag contents)
    (if (number? contents)
        contents
        (cons type-tag contents)))
(define (type-tag z)
    (cond ((number? z) 'scheme-number)
          ((pair? z) (car z))
          (else #f)))
(define (contents z)
    (if (number? z)
        z
        (cdr z)))

(define (apply-generic op . args)
    (let ([type-tags (map type-tag args)]
          [log-err (lambda () 
                    (error
                      "No method for these types: APPLY-GENERIC"
                      (list op (map type-tag args))))])
        (let ([proc (get op type-tags)])
            (if proc
                (drop (apply proc (map contents args)))
                ;; to simplify the exercise, we only
                ;; consider the situation of 2 args
                (if (= (length args) 2)
                    (let ([t1 (car type-tags)]
                          [t2 (cadr type-tags)])
                        (if (same-type? t1 t2)
                            (log-err)
                            (let ([same-args (make-same args)])
                                 (drop 
                                    (apply-generic 
                                        op 
                                        (car same-args) 
                                        (cadr same-args))))))
                    (log-err))))))

(define (same-type? t1 t2) (eq? t1 t2))

;; implement the tower strategy
(define (make-same args)
    (let ([type-tags (map type-tag args)])
        (let ([t1 (car type-tags)]
              [t2 (cadr type-tags)]
              [a1 (car args)]
              [a2 (cadr args)])
            (if (same-type? t1 t2)
                args
                (let ([o1 (get-order t1)]
                      [o2 (get-order t2)])
                      (if (and o1 o2)
                          (if (< o1 o2)
                              ;; super type is on the left side of subtype
                              (make-same (list a1 (raise a2)))
                              (make-same (list (raise a1) a2)))
                          (error 
                              "these types are not in the same tower: MAKE-SAME"
                              type-tags)))))))

;; presume that each type has only one super type
(define (create-tower)
    (define tower '())
    (define (root type)
        (if (null? tower)
            (begin
                (set! tower (list type))
                tower)
            (error
                "root type has already exist: ROOT"
                (list type))))
    (define (put type super)
        (define (iter left rest)
            (if (null? rest)
                #f
                (let ([t (car rest)])
                    (if (same-type? t super)
                        ;; the type will be set behind its super type
                        (begin
                            (set! tower (append left (list super type) (cdr rest)))
                            #t)
                        (iter (append left (list t)) (cdr rest))
                        ))))
        (if (get-order type)
            #t
            (if (get-order super)
                (iter '() tower)
                (error
                  "super type doesn't exist: PUT-TOWER"
                  (list type super)))))
    (define (get-order type)
        (define (iter counter rest)
            (if (null? rest)
                #f
                (if (same-type? type (car rest))
                    counter
                    (iter (+ counter 1) (cdr rest)))))
        (iter 0 tower))
    (lambda (op . args)
        (cond ((eq? op 'root) (apply root args))
              ((eq? op 'put) (apply put args))
              ((eq? op 'get-order) (apply get-order args))
              ((eq? op 'list) tower)
              (error
                "unsupported operation: "
                (list op args)))))

(define arith-tower (create-tower))
(define (root-tower type)
    (arith-tower 'root type))
(define (put-tower type super) 
    (arith-tower 'put type super))
(define (get-order type)
    (arith-tower 'get-order type))

;; create arithmetic tower
(root-tower 'complex)
(put-tower 'rational 'complex)
(put-tower 'scheme-number 'rational)
(arith-tower 'list)

;; generic arithmetic operations
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (install-complex-package)
    (define (real-part z) (apply-generic 'real-part z))
    (define (imag-part z) (apply-generic 'imag-part z))
    (define (magnitude z) (apply-generic 'magnitude z))
    (define (angel z) (apply-generic 'angel z))
    (define (make-from-real-imag x y)
        ((get 'make-from-real-imag '(rectangular)) x y))
    (define (make-from-mag-ang r a)
        ((get 'make-from-mag-ang '(polar)) r a))

    (define (add-complex z1 z2)
        (make-from-real-imag (add (real-part z1) (real-part z2))
                             (add (imag-part z1) (imag-part z2))))
    (define (sub-complex z1 z2)
        (make-from-real-imag (sub (real-part z1) (real-part z2))
                             (sub (imag-part z1) (imag-part z2))))
    (define (mul-complex z1 z2)
        (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                           (sub (angel z1) (angel z2))))
    (define (div-complex z1 z2)
        (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                           (sub (angel z1) (angel z2))))

    (define (tag z) (attach-tag 'complex z))
    (put 'add '(complex complex)
        (lambda (z1 z2) (tag (add-complex z1 z2))))
    (put 'sub '(complex complex)
        (lambda (z1 z2) (tag (sub-complex z1 z2))))
    (put 'mul '(complex complex)
        (lambda (z1 z2) (tag (mul-complex z1 z2))))
    (put 'div '(complex complex)
        (lambda (z1 z2) (tag (div-complex z1 z2))))
    (put 'make-from-real-imag 'complex
        (lambda (x y) (tag (make-from-real-imag x y))))
    (put 'make-from-mag-ang 'complex
        (lambda (r a) (tag (make-from-mag-ang r a))))
    (put 'real-part 'complex real-part)
    (put 'imag-part 'complex imag-part)
    (put 'magnitude 'complex magnitude)
    (put 'angel 'complex angel)

    'done)

(define (install-rational-package)
    (define (make-rat n d) 
        (let ((g (gcd n d)))
            (cons (/ n g) (/ d g))))
    (define (numer r) (car r))
    (define (denom r) (cdr r))
    (define (add-rat x y)
        (make-rat (+ (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (sub-rat x y)
        (make-rat (- (* (numer x) (denom y))
                     (* (numer y) (denom x)))
                  (* (denom x) (denom y))))
    (define (mul-rat x y)
        (make-rat (* (numer x) (numer y))
                  (* (denom x) (denom y))))
    (define (div-rat x y)
        (make-rat (* (numer x) (denom y))
                  (* (denom x) (numer y))))

    (define (tag x) (attach-tag 'rational x))
    (put 'add '(rational rational)
        (lambda (x y) (tag (add-rat x y))))
    (put 'sub '(rational rational)
        (lambda (x y) (tag (sub-rat x y))))
    (put 'mul '(rational rational)
        (lambda (x y) (tag (mul-rat x y))))
    (put 'div '(rational rational)
        (lambda (x y) (tag (div-rat x y))))
    (put 'make-rat 'rational
        (lambda (n d) (tag (make-rat n d))))
    (put 'numer 'rational numer)
    (put 'denom 'rational denom)
    'done)

(define (install-scheme-number-package)
    (define (tag x) (attach-tag 'scheme-number x))
    (put 'add '(scheme-number scheme-number)
        (lambda (x y) (tag (+ x y))))
    (put 'sub '(scheme-number scheme-number)
        (lambda (x y) (tag (- x y))))
    (put 'mul '(scheme-number scheme-number)
        (lambda (x y) (tag (* x y))))
    (put 'div '(scheme-number scheme-number)
        (lambda (x y) (tag (/ x y))))
    (put 'make 'scheme-number
        (lambda (x) (tag x)))
    'done)

(define (install-equal-package)
    (define (equ-ordinary? x y) (= x y))
    (define (equ-rational? x y)
        (let ([numer (get 'numer 'rational)]
              [denom (get 'denom 'rational)])
            (and (= (numer x) (numer y))
                 (= (denom x) (denom y)))))
    (define (equ-complex? x y)
        (let ([real-part (lambda (z) (apply-generic 'real-part z))]
              [imag-part (lambda (z) (apply-generic 'imag-part z))])
            (and (apply-generic 'equ? (real-part x) (real-part y))
                 (apply-generic 'equ? (imag-part x) (imag-part y)))))

    (put 'equ? '(scheme-number scheme-number) equ-ordinary?)
    (put 'equ? '(rational rational) equ-rational?)
    (put 'equ? '(complex complex) equ-complex?)
    'done)

(define (equ? x y) (apply-generic 'equ? x y))

(define (install-zero-package)
    (define (=zero-ordinary? x) (= x 0))
    (define (=zero-rational? x)
        (let ([numer (get 'numer 'rational)])
            (= (numer x) 0)))
    (define (=zero-complex? x)
        (let ([real-part (lambda (z) (apply-generic 'real-part z))]
              [imag-part (lambda (z) (apply-generic 'imag-part z))])
            (and (= (real-part x) 0)
                 (= (imag-part x) 0))))

    (put '=zero? '(scheme-number) =zero-ordinary?)
    (put '=zero? '(rational) =zero-rational?)
    (put '=zero? '(complex) =zero-complex?)
    'done)

(define (=zero? x) (apply-generic '=zero? x))


(define (install-raise-package)
    (define (scheme->rational x)
        ((get 'make-rat 'rational) x 1))
    (define (rational->complex x)
        ((get 'make-from-real-imag 'complex) x 0))

    (put 'raise 'scheme-number scheme->rational)
    (put 'raise 'rational rational->complex)
    'done)

(define (raise x)
    (let ([m (get 'raise (type-tag x))])
        (if m
            (m x)
            (error 
              "no method for these types: RAISE" 
              (list 'raise (type-tag x))))))

(module+ test
    (require rackunit)
    (install-arithmethic-packages)
    (install-zero-package)

    (check-true (=zero? 0))
    (check-false (=zero? 3))

    (define (make-rat n d) ((get 'make-rat 'rational) n d))
    (check-true (=zero? (make-rat 0 1)))
    (check-false (=zero? (make-rat 3 4)))
    
    (define (make-from-real-imag x y) ((get 'make-from-real-imag 'complex) x y))
    (check-true (=zero? (make-from-real-imag 0 0)))
    (check-false (=zero? (make-from-real-imag 3 4)))
    )

(define (install-arithmethic-packages)
    (install-rectangular-package)
    (install-polar-package)
    (install-complex-package)
    (install-rational-package)
    (install-scheme-number-package)
    (install-raise-package)
    (install-equal-package)
    (install-zero-package)
    (install-project-package)
    'done)

(module+ main
    (install-arithmethic-packages)
    (define make-from-real-imag (get 'make-from-real-imag 'complex))
    (define make-from-mag-ang (get 'make-from-mag-ang 'complex))
    (define make-rat (get 'make-rat 'rational))
    (define (rational? x) 
        (and (not (number? x))
             (eq? (type-tag x) 'rational)))
    (define (numer x) ((get 'numer 'rational) (contents x)))
    (define (denom x) ((get 'denom 'rational) (contents x)))
    (define (complex? x)
        (and (not (number? x))
             (eq? (type-tag x) 'complex)))
    (define (real-part z)
        ((get 'real-part 'complex) (contents z)))
    (define (imag-part z)
        ((get 'imag-part 'complex) (contents z)))
    
    (drop #t)
    (drop (make-from-real-imag 1 2))
    (drop (make-from-real-imag 1 0))
    (drop (make-from-real-imag (make-rat 2 1) 0))
    (add (make-rat 2 3) (make-from-real-imag 2 3))
    )