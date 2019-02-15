#| The high order logic of these two functions is implemented through symmetry. At each max/min node, we have a collection of min/max child nodes to examine,
respectively. If the child node is a leaf node, the implementation simply returns it's value along with the number 1 to indicate that 1 node was examined. 
If not, the implementation calculates the first child's result (calling minimax from maximin or vice versa) and adjusts the (alpha for maximin) or (beta for minimax)
values. If alpha >= beta, then the functions simply return the first child's result and effectively prune. If alpha < beta, then a recursive call to the same 
top-level function is made to compute the minimax/maximin of the rest of the children. We can see this is a top-down approach. Once the remaining children are 
evaluated, the top-level function returns two values as a list: first is the (max for maximin, min for minimax) of the first of the two return values of the first 
and remaining children, and secondly the sum of the number of nodes examined for the first child and remaining children. |# 

(defun MAXIMIN (lst alpha beta) "MAXIMIN function that completes minimax search assuming root node is a MAX node"
    (cond ((atom lst) (list lst 1))                                                                         ;; if a leaf node just return list containing its value and 1
        (t  (let ((first (MINIMAX (car lst) alpha beta)))                                                   ;; examine first child of list of children
                (cond ((null (cdr lst)) first)                                                              ;; if no other children, return the result of the first child 
                    (t  (let ((newalpha (max alpha (car first))))                                           ;; else, adjust alpha* for max node 
                            (cond ((>= newalpha beta) first)                                                ;; if alpha >= beta, no need to examine further nodes -> can prune and just return first child result 
                                (t  (let ((rest (MAXIMIN (cdr lst) newalpha beta)))                         ;; else, run the same MAXIMIN* search on the remaining children with adjusted alpha* 
                                        (list (max (car first) (car rest)) (+ (cadr first) (cadr rest)))    ;; first element of return list is maximum* child candidate, second element is addition of all nodes examined 
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)

(defun MINIMAX (lst alpha beta) "MINIMAX function that completes minimax search assuming root node is a MIN node"
    (cond ((atom lst) (list lst 1))                                                                         ;; if a leaf node just return list containing its value and 1
        (t  (let ((first (MAXIMIN (car lst) alpha beta)))                                                   ;; examine first child of list of children
                (cond ((null (cdr lst)) first)                                                              ;; if no other children, return the result of the first child 
                    (t  (let ((newbeta (min beta (car first))))                                             ;; else, adjust beta* for min node 
                            (cond ((>= alpha newbeta) first)                                                ;; if alpha >= beta, no need to examine further nodes -> can prune and just return first child result
                                (t  (let ((rest (MINIMAX (cdr lst) alpha newbeta)))                         ;; else, run the same MINIMAX* search on the remaining children with adjusted beta* 
                                        (list (min (car first) (car rest)) (+ (cadr first) (cadr rest)))    ;; first element of return list is minimum* child candidate, second element is addition of all nodes examined 
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )
    )
)