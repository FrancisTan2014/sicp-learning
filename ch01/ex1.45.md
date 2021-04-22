## Exercise 1.45
We saw in Section 1.3.3 that attempting to
compute square roots by naively finding a fixed point of
y → x/y does not converge, and that this can be fixed by
average damping. The same method works for finding cube
roots as fixed points of the average-damped y 7→ x/y2
. Unfortunately, the process does not work for fourth roots—a
single average damp is not enough to make a fixed-point
search for y → x/y3
converge. On the other hand, if we
average damp twice (i.e., use the average damp of the average damp of y → x/y3
) the fixed-point search does converge. Do some experiments to determine how many average damps are required to compute n
th roots as a fixed-point search based upon repeated average damping of y → x/yn−1
. Use this to implement a simple procedure for computingn
th roots using fixed-point, average-damp, and the
repeated procedure of Exercise 1.43. Assume that any arithmetic operations you need are available as primitives.

## Experiments
Let's experiment first. Find out the pattern where the average number of damping changes with the exponential increase of x.  

Obviously, the procedure **repeated** that was defined in the **Exercise 1.43** can be used here to define a function **average-damp-n**:
```Lisp
(define (average-damp f) 
    (lambda (x) 
        (average x (f x))))
(define (average-damp-n f n)
    ((repeated average-damp n) f))
```

To find the fourth root:
```Lisp
(define (sqrt-4 x)
    (fixed-point (average-damp-n (lambda (y) (/ x (cube y)))
                                 2)
                 1.0))

(sqrt-4 16) ; expexted to get 2
```

## Automatical
Then how about the 5th root? Before the action, let's build some tool precedures to help us do the experiments automatically.

But before that was done, I have to learn the knowledge of thread in the language Racket. So I stopped doing this.

## Testing
```Lisp
(define (root-of x nth damp-builder [first-guess 1.0])
    (define f (λ (y) (/ x (expt y (- nth 1)))))
    (fixed-point (damp-builder f)
                 first-guess))
```

Execute the **root-of** procedure for several times, I got:
Nth | Times of average-damp
---|---
2 | 1
3 | 1
4 | 2
... | ...
8 | 3
9 | 3
... | ...
16 | 4
17 | 4
... | ...
32 | 5
33 | 5
... | ...
64 | 6
65 | 6
... | ...

## The Common Definition
Obviously, ```times=(floor (log n 2))```. That is the pattern we were looking for. Now we can define a common procedure to compute the **nth** root of number x:
```Lisp
(define (nth-root x n)
    (root-of x n (average-damp-n (floor (log n 2)))))
```