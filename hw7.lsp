
#| The methodology used for creating this algorithm was reminiscent of the General Problem Solver. When given an 
initial and goal state, we first want to move the next largest disk to its goal peg. This is key, due to the 
fact that when it arrives in it's goal state, it never needs to be moved again, and because it is the largest 
disk (and the larger disks before it also don't need to be moved, by induction), we know that moving all of the 
subsequently smaller disks can be done without ever taking this largest disks into account ever again. Thus, we can 
further prune the problem into placing the smaller disks into their goal locations, as if the larger disks never existed. 

The state representation I will be using is a singular list of size N, where N equals the number of disks. Each 
index of the list will be filled with either A, B, or C denoting that the disk assigned to that
index is at peg A, B, or C. It is important to note that the pegs are ordered from largest to smallest in the 
list, so the indices are effectively reversed and the first element of the list corresponds to the peg of the largest disk.
It is also convenient that at any point, the size of the list is the disk number of the next largest disk to be placed. 
Similarly, the goal state will be the same kind of list, but with the disks at their goal locations. At any point, 
we can see the current peg and the goal peg of the next largest disk by looking at the first elements of the initail state 
and goal state lists respectively. 

The state representation is a key factor in this solution. At any point, the first element of the state list is the 
next largest peg to be moved, and its goal peg is the first element of the goal list. If the next largest disk is 
already in its goal peg, then the algorithm just returns the solution of assigning the remainder of the state list
to the remainder of the goal list, which represents the goal pegs all of the smaller pegs. 

If the largest peg is NOT in its goal peg, we need to move ALL of the smaller pegs to the auxiliary peg (the peg that is 
neither the goal nor starting peg of the largest disk) before we can move the largest disk to its appropriate peg. This is 
true in all cases. We know that we don't need to worry about the larger disks, because they do not cause any conflicts due 
to their size and they will never need to be moved again. However, imagine if there was some smaller disk that occupied either 
the starting or goal peg. If it were on the starting peg, we know it is above our largest disk because of the assumed validity 
of the state, and so we cannot move the larger disk yet. If it is on the goal peg, we cannot place the larger peg on top of it, 
or it would transition to an invalid state. 

Thus, a subproblem for moving the largest disk to its goal peg is moving all of the smaller disks to the auxiliary peg. Once we 
solve this through a recursive call, we can now move the largest peg without moving to an invalid state, and now the problem breaks
down into assigning all of the smaller pegs to their goal states assuming they are all currently resting on the auxiliary peg used. 
This recursive call makes use of a helper function to generate the new initial state of all of the small disks being on the auxiliary 
peg, and the goal state is simply the cdr of the goal list due to the nature of our reverse sorted list. |#

(defun solve (state goal)	"General solver function that attempts to place next largest peg and then all remaining pegs recursively"
	(cond ((null goal) nil)																			;; if no more pegs to move, return no further operations 
		(t 	(let ((NLDIndex (car state))															;; current peg of next largest disk 
				  (NLDGoal (car goal)))																;; goal peg of next largest disk 
				(cond ((equal NLDIndex NLDGoal) (solve (cdr state) (cdr goal)))						;; if already at goal, return solution of moving remaining disks 
					(t	(let* ((auxiliaryPeg (findAuxiliary NLDIndex NLDGoal))						;; auxiliary peg 
							   (diskNumber (length state))											;; largest disk number is length of list 
							   (auxiliaryGoal (createAuxiliaryGoal auxiliaryPeg (- diskNumber 1))))	;; create goal state for moving smaller disks
							(append (solve (cdr state) auxiliaryGoal) (list (list diskNumber NLDIndex NLDGoal)) (solve auxiliaryGoal (cdr goal)))	
							;; append the solution of moving the smaller disks to the operation of moving the next largest disk to the 
							;; solution of reassigning the remaining smaller pegs, assuming they now all rest on the auxiliary peg
						)
					)
				)
			)
		)
	)
)

(defun createAuxiliaryGoal (auxiliaryPeg num)  "Helper function that simply creates a list of size num filled with auxiliaryPeg at each index"
	(cond ((zerop num) nil)
		(t (cons auxiliaryPeg (createAuxiliaryGoal auxiliaryPeg (- num 1))))
	)
)

(defun findAuxiliary (NLPIndex NLPGoal)	"Helper function to return the auxiliary peg given the current and goal pegs of the next largest disk"
	(cond ((equal NLPIndex 'A) (cond ((equal NLPGoal 'B) 'C) (t 'B)))
		  ((equal NLPIndex 'B) (cond ((equal NLPGoal 'C) 'A) (t 'C)))
		(t (cond ((equal NLPGoal 'B) 'A) (t 'B)))
	)
)

(defun test1 ()
	(solve '(A B C A) '(C B A C))
)

(defun test2 ()
	(solve '(C B A) '(A B C))
)

(defun test3 ()
	(solve '(A A A) '(C C C))
)

(defun test4 ()
	(solve '(A B C C B) '(C B A A B))
)

(defun test5 ()
	(solve '(A B C A B C) '(A C B C B A))
)

(defun test6 ()
	(solve '(B A B A B A) '(A B A B A B))
)

(defun test7 ()
	(solve '(A B B C C C) '(A C C A B A))
)

(defun test8 ()
	(solve '(A A A A A A A) '(C C C C C C C))
)

(defun test9 ()
	(solve '(C C B B B A A A A) '(C C B B B A A A A))
)

(defun test10 ()
	(solve '() '())
)

(defun test11 ()
	(solve '(B B B A A A A) '(A C A C A C A))
)

(defun test12 ()
	(solve '(B C C B B A A A) '(C A B C B A B C))
)