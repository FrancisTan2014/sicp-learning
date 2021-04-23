## Exercise 2.4
Here is an alternative procedural representation of pairs. For this representation, verify that (car (cons
x y)) yields x for any objects x and y.
```Lisp
(define (cons x y)
    (lambda (m) (m x y)))
```

What is the corresponding definition or cdr? (Hint: To verify that this works, make use of the substitution model of
**Section 1.1.5**.)

## Verification
According to the definition of **cons**, cons takes x and y as the arguments and returns a procedure that takes a procedure as the argument.
```Lisp
(define (car z)
    (z (lambda (p q) p)))
```

Use the substitution model of Section 1.1.5, we can expand the definition of car as:
```Lisp
((lambda (m) (m x y)) (lambda (p q) p))
((lambda (p q) p) x y)
; so the result is x
```

Correspondingly, the definition of cdr is:
```Lisp
(define (cdr z)
    (z (lambda (p q) q)))
```