#| The below DFS function is implemented in the following manner. We car through the list
and check if the car'd element is an atom. If so, append it with the DFS of the remainder (cdr)
of the list. If it is not an atom, append the DFS of the car'd elemend to the DFS of the remainder
(cdr) |#

(defun DFS(lst) "Depth first search function that returns list of atoms in order of DFS iteration through list"
    (cond ((NULL lst) ())
        (t (let ((first (car lst)))
                (cond ((ATOM first) (cons first (DFS (cdr lst))))   ;; If an atom, then just append it to the DFS of the remainder
                    (t (append (DFS first) (DFS (cdr lst))))        ;; Otherwise, recursively DFS into this list and append the result to the DFS of the remainder
                )
            )
        )
    )
)






#| The depth-first iterative deepening (DFID) function works in by calling a DOS function that returns
a list of all atoms within the first n levels. The DFID function calls the function recursively and appends
the returned lists strategically by appending the recursive call of the next lower later with the elements 
returned by a DOS call on the current layer |#


(defun DFID(lst n)  "Depth-first iterative deepening that recursively calls DOS helper function to build list from top down"
    (cond ((zerop n) nil)
        (t (append (DFID lst (- n 1)) (DOS lst n))) ;; append the DFID of previous depths with the atoms found within max depth n
    )
) 

(defun DOS(lst n) "Returns list of atoms within lst with depth <= n"
    (cond ((NULL lst) ())
          ((zerop n) ())
        (t (let ((first (car lst)))
            (cond ((ATOM first) (cons first (DOS (cdr lst) n)))     ;; If an atom, append it to the DOS of the remainder
                (t (append (DOS first (- n 1)) (DOS (cdr lst) n)))  ;; Otherwise, recursively append the DOS of the list (with n - 1) with the DOS of the rest
            )
           )
        )
    )
)






#| The BFS function works in the following manner. We car through the list and check if the 
car'd element is an atom. If it is, then we simply return the list returned by a cons call with
the car'd atom and a BFS of the remainder (cdr) of the list. If the car'd element is not an atom, 
then we essentially flatten the element by one degree and append it to the end of the list, and then 
recursively call BFS on the resulting list. This is effective, as it preserves the BFS order by 
appending to the end of the list. |# 

(defun BFS(lst) "Breadth first search function that returns list of atoms in order of BFS iteration through list"
    (cond ((NULL lst) ())
        (t (let ((first (car lst)))
                (cond ((ATOM first) (cons first (BFS (cdr lst))))   ;; If an atom, then just append it to the BFS of the remainder
                    (t (BFS (append (cdr lst) first))               ;; Otherwise, append the flattened list to the end of the list
                    )
                )
            )
        )
    )
)