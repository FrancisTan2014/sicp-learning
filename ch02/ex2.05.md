## Exercise 2.5
Show that we can represent pairs of nonnegative integers using only numbers 
and arithmetic operations if we represent the pair a and b as the integer that is 
the product **2^a * 3^b**. Give the corresponding definitions of the procedures 
**cons**, **car**, and **cdr**.

## Analyzing
According to the description above, the cons may be defined as a procedure that takes a and b as the arguments, and computes the product of **2^a * 3^b**(names p), and returns a procedure that will compute **a or b** from the product when gives **2 or 3** as the argument. So that the definitions will be like:
```Lisp
(define (cons a b)
    (let ((p <the product>))
        (lambda (i)
            (cond ((= i 2) <compute a>)
                  ((= i 3) <compute b>)))))

(define (car p) (p 2))
(define (cdr p) (p 3))
```

Now the problem is to find out the definitions of product and compute which can only use +,-,*,/ as the operations.

The **product** procedure would be easy:
```Lisp
(define (product a b)
    (define (iter i n base result)
        (if (> i n)
            result
            (iter (+ i 1) n base (* base result))))
    (* (iter 1 a 2 1)
       (iter 1 b 3 1)))
```

How about the **compute** procedure?

## Abstraction Barriers
\+ \- \* \/ -> power,mod,log