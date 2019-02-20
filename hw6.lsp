#| In order to implement the QUEENS function, I made use of two auxiliary function, one to place the queens and backtrack, and another 
to see if placing a certain queen at a certain index would conflict with previous placements. The overall QUEENS function calls the 
helper function with the arguments nil, 1, 1, and n. This means it is asking helper to start with an empty list of queens placed, and 
try to place the first queen at index 1, and only try to place n queens. The helper function attempts to place the current queen, denoted
by the "next" variable, and if there are no conflicts with any previous placements, attempts to place the remaining queens with the current 
queen at the current index. If there is a conflict or the remaining queens can't be placed without conflict, we recursively call the helper
function with index + 1 to attempt the process again but with placing the current queen in the next row. If we run out of rows to check, in 
which case index > n, we return the 'fail atom to denote a failure to place the current queen anywhere given the currently previously placed
queens. This is when we backtrack, and try to re-place the previous queen. When the "next" variable > n, we have placed all of the queens, so 
we return an empty list due to the nature of us appending the current indices to the remainder of the solution. The overall helper function 
makes use of the isLegal auxiliary function that essentially checks if placing the current queen at a certain index would cause conflicts with
previous placements. |#

(defun QUEENS (n) "Queens function that outputs row values for each respective column for the n-Queens placement given n"
	(HELPER nil 1 1 n)	;; make call to helper function with n = n,  lst = nil, try index 1 first, and 1 is the next queen to be placed
)


(defun helper (lst index next n)
	(cond ((> next n) nil)	;; if next queen to be placed is n+1th, then we've finished so just return empty list
		  ((> index n) 'fail)	;; if trying to position on the n+1th row, we've run out of rows to try, return fail
		(t (let ((possiblePlacement (isLegal lst index 1 next)))	;; possiblePlacement = true if placing next queen at row = index does not caus any problems 
			 (cond ((equal possiblePlacement nil) (helper lst (+ index 1) next n))	;; if there is a conflict, try the next row with the same queen
			 	(t (let ((restOfPlacements (helper (append lst (list index)) 1 (+ next 1) n)))	;; else, try placing the rest of the queens with the current queen at index 
				 	(cond ((equal restOfPlacements 'fail) (helper lst (+ index 1) next n))	;; if not possible to position rest of queens, try the next row for this queen and recurse
						(t (cons index restOfPlacements))	;; if it was possible, a list of row indexes for the remaining queens is returned, append the current index to the list and return 
					)
				   )
				)
			 )
		   )
		)
	)
)


(defun isLegal (lst indexOfPlacement indexOfCheck queenNumber) "Helper function to see if placing the queenNumber'th queen at indexOfPlacement causes a conflict with any of the queen placements in lst"
	(cond ((null lst) t)	;; if the lst is null, no conflicts 
		(t (let ((first (car lst)))	;; get the first queen placement in the lst 
			(cond ((= first indexOfPlacement) nil)	;; if queens are in same row, there is a conflict so return nil 
				  ((equal (abs (/ (- queenNumber indexOfCheck) (- indexOfPlacement first ))) 1) nil)	;; if path between queens has slope = +- 1, then there is diagonal conflict, so return false
				(t (isLegal (cdr lst) indexOfPlacement (+ indexOfCheck 1) queenNumber))	;; else this column is legal, see if the remaining are as well 
			)
		   )
		)
	)
)