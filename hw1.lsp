#| The below FIB function works, but is very slow and has an exponential runtime complexity due to recomputation of the same values.
The function works relatively quickly (in regards to human perception of time) for values lower than 20, but it gradually takes longer
and longer to compute the larger values. When the above function is invoked on even larger values of n, the runtime explodes and it takes 
very long to calculate the number recursively. The reason for this is, as stated before, no memoization along with top down recursion, 
which causes needless computation. |#

(defun FIB(n) "Returns nth number from the Fibonacci sequence"
    (cond ((equal n 1) 1)   ;; base cases
          ((equal n 2) 1)
        (t (+ (FIB (- n 1)) (FIB (- n 2)))) ;; add the previous two Fibonacci numbers
    )
)






#| The SUMS function understands that the FIB function is inefficient, and each recursion takes it's own number of additions even if they 
had already been seen in other recursive calls. Because we add the results of two recursive calls, the number of additions is 
(1 + # of additions for first recursion + # of additions for second recursion). The base cases are the first 2 Fibonacci numbers, where my
function requires no additions. |#

#|The first 10 values returned from my SUMS function are {0, 0, 1, 2, 4, 7, 12, 20, 33, 54}. The first 10 values from the FIB function 
are {1, 1, 2, 3, 5, 8, 13, 21, 34, 55}. It seems that the values returned by FIB are simply the values returned by SUMS + 1. This makes sense,
as when we look at the structure of both functions, they are essentially the same recursive function, with some different initialization and base case 
return values. |#

(defun SUMS(n) "Returns number of additions necessary for FIB to compute nth Fibonacci number"
    (cond ((equal n 1) 0)   ;; base cases
          ((equal n 2) 0)
        (t (+ 1 (+ (SUMS (- n 1)) (SUMS (- n 2))))) ;; 1 addition + the number required to compute the previous two Fibonacci numbers
    )
)








#| The theory behind the FASTFIB function is that we can recursively construct the nth Fibonacci number by building up from the first 2. An 
auxiliary function is used in this procss that accepts a list that contains the base-case-dependent variable n and the two numbers to recursively
build from. It is essentially a way to model an iterative bottom up iterative approach, as the function argmuments essentially serve to be storage. |#

#| Looking at the runtimes of the first 20 or so Fibonacci numbers, it seems that each iteration's runtime increases by a factor of about 1.6. Therefore,
I would expect FIB(20) to take 1.6^5 = ~ 10x amount of time as FIB(15). This actually is semi-accurate, as the times recorded for FIB(15) and FIB(20) are 
0.025943 and 0.252629 respectively. From this, I would calculate the estimated amount of time to calculate FIB(100) by calculating FIB(30) and multiplying 
this by 1.6^70. This gives us about 1.94e14 * 32.32 seconds = ~ 200 million years. |#


(defun FASTFIB(n) "Returns nth Fibonacci number using only n additions"
    (FIBHELPER (list n 0 1))    ;; call to helper function that does bottom up recursion
)

(defun FIBHELPER(lst) "Helper function for FASTFIB that computes Fibonacci based on last two numbers (bottom up recursion)"
    (let ((n (car lst))                                             ;; extract the first element, n, from the lst
          (rest (cdr lst)))                                         ;; save the remaining elements inside "rest"
        (let ((ft1 (car rest))                                      ;; get the next element, ft1, from the list
              (rest (cdr rest)))                                    ;; save new rest instance for final list element
            (let ((ft2 (car rest)))                                 ;; extract ft2 from the list
                (cond ((zerop n) ft1)                               ;; if n is 0, return the first of the two numbers
                    (t (FIBHELPER (list (- n 1) ft2 (+ ft1 ft2))))  ;; now with all elements, recursively build bottom-up solution
                )
            )
        )
    ) 
)
