

(defvar *goal-state* '(0 1 2 3 4 5 6 7 8))

(defvar *operators* '((1 3) (0 2 4) (1 5) (0 4 6) (1 3 5 7) (2 4 8) (3 7) (4 6 8) (5 7)))

(defvar *manhattan-distances* 
  '((0 1 2 1 2 3 2 3 4)	
    (1 0 1 2 1 2 3 2 3)
    (2 1 0 3 2 1 4 3 2)
    (1 2 3 0 1 2 1 2 3)
    (2 1 2 1 0 1 2 1 2) 
    (3 2 1 2 1 0 3 2 1) 
    (2 3 4 1 2 3 0 1 2) 
    (3 2 3 2 1 2 1 0 1) 
    (4 3 2 3 2 1 2 1 0))
)

(defun ida* (state)
    (DFID state (get-heuristic state) nil)
)


(defun DFID(state max-depth visited-states)     "DFID function to return sequence of tiles to push to reach goal state"
    (cond
        ((null state)  "fail")                                                                          ;; edge case/base condition
        ((equal state *goal-state*) nil)                                                                ;; if goal state, no more moves 
        ((zerop max-depth) "fail")                                                                      ;; if we've exhausted our depth limit, return "fail"
        ((equal (mod (count-inversions state) 2) 1) "fail")                                             ;; if number of inversions in state is odd, unsolvable
        ((is-redundant-state state visited-states) "fail")                                              ;; don't process redundant states 
        (t (let* ((children-states-with-operations (get-children-states-with-operations state))         ;; get children states with their linked operations from current state
                  (results (results-of-children children-states-with-operations (- max-depth 1) (cons state visited-states)))   ;; get results of recursing on each of children states 
                  (best-child (find-success results 0)))                                                ;; get the "best result" or rather any success amongst recursive calls
                (cond ((null best-child) "fail")                                                        ;; if no success, failure
                    (t (cons (get-operation-from-child (get-Nth children-states-with-operations best-child)) (get-Nth results best-child))) ;; otherwise, append the operation to the list of operations returned by the recursive call
                )
            )
        )
    )
)

(defun get-heuristic (state)    "Auxiliary function to get heuristic measure, essentially a wrapper function for get-manhattan-distances starting from index 0"
    (get-manhattan-distances state 0)
)

(defun get-manhattan-distances (state index)    "Auxiliary function to calculate and add the Manhattan distances of each tile from their position in the goal state"
    (cond ((null state) 0)
          ((zerop (car state)) (get-manhattan-distances (cdr state) (+ index 1)))                           ;; don't calculate Manhattan distance for blank tile
        (t (+ (manhattan-distance (car state) index) (get-manhattan-distances (cdr state) (+ index 1))))    ;; recursively add Manhattan distance of each tile 
    )
)

(defun manhattan-distance (number index)    "Auxiliary function to get Manhattan distance of certain tile from its location in goal state"
    (get-Nth (get-Nth *manhattan-distances* number) index)
)

(defun count-inversions (state)     "Auxiliary function to count number of inversions within a state"
    (cond ((null state) 0)
        (t (+ (count-inversions-singular (car state) (cdr state)) (count-inversions (cdr state))))  ;; recursively add all of the inversion counts of each individual tiles in the state 
    )
)

(defun count-inversions-singular (reference rest) "Auxiliary function to count number of inversions with regard to a reference tile and all of the tiles 'after' it"
    (cond ((null rest) 0)
          ((zerop reference) 0)                                                                 ;; don't count inversions for the blank tile as a reference tile
          ((zerop (car rest)) (count-inversions-singular reference (cdr rest)))                 ;; don't count the blank tile as an inversion against some other reference
          ((> reference (car rest)) (+ 1 (count-inversions-singular reference (cdr rest))))     ;; if reference > other element, it is an inversion
        (t (count-inversions-singular reference (cdr rest)))                                    ;; otherwise, check the rest of the remaining list for inversions against the same reference
    )
)

(defun is-redundant-state (state visited-states)    "Auxiliary function to see if state is within visited-states"
    (cond ((null visited-states) nil)
          ((equal state (car visited-states)) t)
        (t (is-redundant-state state (cdr visited-states)))
    )
)

(defun find-success (results index)     "Auxiliary function to search the results of recursing on all children for any success"
    (cond ((null results) nil)
           ((equal (car results) nil) index)        ;; if result is nil, then it is the goal state 
           ((not (atom (car results))) index)       ;; if result is a list, then it is a path to the goal state
        (t (find-success (cdr results) (+ index 1)))
    )
)

(defun results-of-children (children-states-with-operations max-depth visited-states)  "Auxiliary function to compute the results of recursing on all children states up to max depth"
    (cond ((null children-states-with-operations) nil)
        (t (cons (DFID (get-state-from-child (car children-states-with-operations)) max-depth visited-states) (results-of-children (cdr children-states-with-operations) max-depth visited-states)))
    )
)

(defun get-operation-from-child (child)     "Auxiliary helper function that returns the operation from a (operation, resulting state) tuple"
    (car child)
)

(defun get-state-from-child (child)     "Auxiliary helper function that returns the state from a (operation, resulting state) tuple"
    (car (cdr child))
)

(defun get-children-states-with-operations (state)     "Auxiliary function to get all possible children of a current state"
    (let* ((blank-index (get-blank-index state))                                                            ;; first, get the index of the blank tile
           (operations (get-Nth *operators* blank-index))                                                   ;; from the blank-index, obtain the list of operators
           (possible-switches (get-possible-switches state operations))                                     ;; get the tile numbers of what is at each of the possible operators
           (children-states (get-children-states state possible-switches))                                  ;; obtain the children states by swapping each of the possible-switches with the blank tile
           (children-with-operations (append-operation-to-children possible-switches children-states)))
        children-with-operations
    )
)

(defun append-operation-to-children (switches children) "Auxiliary function that returns a new list that is the append of each child state with it's origin operator"
    (cond ((null switches) nil)
          ((null children) nil)
        (t (cons (list (car switches) (car children)) (append-operation-to-children (cdr switches) (cdr children))))
    )
)

(defun get-children-states (state operators)    "Auxiliary function to get children states, given a current state and the possible tiles to switch with given by operators"
    (cond ((null state) nil)
          ((null operators) nil)
        (t (cons (switch-tiles state (car operators)) (get-children-states state (cdr operators)))) ;; continuously append the state of resulting switch with recursive call to rest of operators
    )
)

(defun switch-tiles (state operator)    "Auxiliary function to return the state when the blank space is switched with the tile numbered by operator"
    (cond ((null state) nil)
           ((equal (car state) 0) (cons operator (switch-tiles (cdr state) operator)))  ;; if current tile is 0, switch it with the operator
           ((equal (car state) operator) (cons 0 (switch-tiles (cdr state) operator)))  ;; if current tile is operator, switch it with 0
        (t (cons (car state) (switch-tiles (cdr state) operator)))                      ;; else just append and continue
    )
)

(defun get-possible-switches (state operations) "Auxiliary function to get the numbers of the tiles that the blank tile can switch with"
    (cond ((null state) nil)
          ((null operations) nil)
        (t (cons (get-Nth state (car operations)) (get-possible-switches state (cdr operations))))
    )
)

(defun get-blank-index (state)  "Auxiliary function to get the index of the blank tile"
    (cond ((null state) nil)
          ((zerop (car state)) 0)
        (t (+ 1 (get-blank-index (cdr state))))
    )
)

(defun get-Nth (lst n) "Auxiliary function to get Nth item from lst"
    (cond ((null lst) nil)
          ((zerop n) (car lst))
        (t (get-Nth (cdr lst) (- n 1))))
)