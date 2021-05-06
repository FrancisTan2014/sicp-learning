## Exercise 2.22
Louis Reasoner tries to rewrite the first squarelist procedure of **Exercise 2.21** so that it evolves an iterative process:
```Lisp
(define (square-list items) 
    (define (iter things answer) 
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons (square (car things))
                        answer))))
    (iter items nil))
```
Unfortunately, defining square-list this way produces the
answer list in the reverse order of the one desired. Why?
Louis then tries to fix his bug by interchanging the arguments to cons:
```Lisp
(define (square-list items)
    (define (iter things answer)
        (if (null? things)
            answer
            (iter (cdr things)
                  (cons answer
                        (square (car things))))))
    (iter items nil))
```

## Question 1
Each time the **iter** executes will cause the first item of things be the last element of the result list. So that we'll get a reversed list at last.

## Question 2
I executed the second **square-list** procedure with list **(1 2 3 4)** but got **((((() . 1) . 4) . 9) . 16)** as the result. Obviously, Louis has not fixed the "reversal" bug yet. The issue is simplely caused by the use of **cons**. Each time cons was called, we got a pair of whose **car** part if a list and **cdr** part is a number.

## Solution
If we break the context of Lisp, the issue above can be fixed by calling a **insertLast** procedure that takes a list and a data object and inserts the data object to the last position of the list specified(before nil) at each loop.
So I think the next section may introduce this feature of Lisp.