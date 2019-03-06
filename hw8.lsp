(defun refute (lst)	"Initial function to refute a list of clauses, calls auxiliar helper function"
	(refuteHelper lst 0 1 nil)
)

#| I'm sorry about how ugly this top-level helper function looks, I tried to indent to make it easier, and hopefully the comments should clear things up |#

(defun refuteHelper(lst indexOne indexTwo resolutions)	"Helper function for refuting a sequence of clauses" 
	(cond ((>= indexOne (length lst)) 'fail)	;; if indexOne is greater than list, we've gone through all resolutions
		  ((>= indexTwo (length lst)) (refuteHelper lst (+ indexOne 1) (+ indexOne 2) resolutions))	;; if indexTwo exceeds list size, advance first pointer
		(t 	(let* ((first (getNth lst indexOne))
				   (second (getNth lst indexTwo))
				   (resolution (resolve first second)))
				(cond ((equal resolution 'fail) (refuteHelper lst indexOne (+ indexTwo 1) resolutions)) ;; if can't resolve, move to next possible resolution
					  ((null resolution) ;; success !
						(cond ((null resolutions) (list nil (list first) (list second)))	;; if no resolutions so far, no need to chain resolutions
							(t 	(let ((firstPreviousResolution (findInResolutions resolutions first)) (secondPreviousResolution (findInResolutions resolutions second)))
									(cond 	((null firstPreviousResolution) ;; if no resolution path to get to first 
												(cond ((null secondPreviousResolution) (list nil (list first) (list second)))	;; if no resolution path to second too, no chaining resolutions needed
													(t (list nil (list first) (cons second (cdr secondPreviousResolution))))	;; must chain second resolution if resolution path found 
												)	
											)
											(t 	(cond ((null secondPreviousResolution) (list nil (list second) (cons first (cdr firstPreviousResolution))))	;; if first but no second resolution, only chain first
													(t (list nil (cons first (cdr firstPreviousResolution)) (cons second (cdr secondPreviousResolution))))	;; must chain both first and second resolutions 
											)))))))
					(t 	(let ((newLst (cons resolution (deleteElement (deleteElement lst first) second)))	;; new list with new resolutions and without old clauses 
							  (firstPreviousResolution (findInResolutions resolutions first)) (secondPreviousResolution (findInResolutions resolutions second)))	;; finding previous resolution paths for clauses 
								(cond 	((null firstPreviousResolution) ;; no first resolution path 
										(cond ((null secondPreviousResolution) ;; no second either 
												(let ((res (refuteHelper newLst 0 1 (cons (list resolution (list first) (list second)) resolutions))))	;; must add brand new base resolution to list, and recurse
													(cond ((equal res 'fail) (refuteHelper lst (+ indexOne 1) (+ indexOne 2) resolutions))	;; if failure, try next possible resolution
														(t res)	;; return good results 
													)
												)
											  )
											  (t (let ((res (refuteHelper newLst 0 1 (cons (list resolution (list first) secondPreviousResolution) resolutions)))) ;; can chain second resolution and add to list, and recurse
													(cond ((equal res 'fail) (refuteHelper lst (+ indexOne 1) (+ indexOne 2) resolutions)) ;; if failure, try next possible resolution
														(t res) ;; return good results
													)
												)
											  )
										)
									)
									(t (cond ((null secondPreviousResolution) 
												(let ((res (refuteHelper newLst 0 1 (cons (list resolution firstPreviousResolution (list second)) resolutions)))) ;; can chain first resolution and add to list, and recurse 
													(cond ((equal res 'fail) (refuteHelper lst (+ indexOne 1) (+ indexOne 2) resolutions)) ;; if failure, try next possible resolution
														(t res) ;; return good results 
													)
												)
											  )
											  (t (let ((res (refuteHelper newLst 0 1 (cons (list resolution firstPreviousResolution secondPreviousResolution) resolutions)))) ;; can chain both resolutions and add to list, and recurse 
													(cond ((equal res 'fail) (refuteHelper lst (+ indexOne 1) (+ indexOne 2) resolutions)) ;; if failure, try next possible resolution
														(t res) ;; return good results 
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
			)
		)
	)

(defun findInResolutions(resolutions clause) "Helper function to find a resolution path within resolutions that lead to the creation of clause"
	(cond ((null resolutions) nil)
	      ((equal (car (car resolutions)) clause) (car resolutions))
		(t (findInResolutions (cdr resolutions) clause))
	)
)

(defun resolve (lstOne lstTwo)	"Helper function for resolving two clauses"
	(resolveHelper lstOne lstTwo 0)
)

(defun resolveHelper (lstOne lstTwo index)	"Helper function for resolve called by resolve that keeps an index of resolution"
	(cond ((equal index (length lstOne)) 'fail)	;; We've looked at every element in lstOne, no resolution possible
		(t	(let ((element (getNth lstOne index)))
				(cond 	((isNegated element) 	;; if element is negated, look for non-negated element in other list
							(let ((found (searchForElement (car (cdr element)) lstTwo)))
								(cond ((null found) (resolveHelper lstOne lstTwo (+ index 1)))	;; if not found, continue searching 
									(t (combineAndStrip lstOne lstTwo (car (cdr element))))		;; else, combine the clauses and strip repetitions
								)
							)
						)
					(t (let ((found (searchForNegatedElement element lstTwo)))	;; if element is not negated, look for negated element 
								(cond ((null found) (resolveHelper lstOne lstTwo (+ index 1)))	;; if not found, keep searching
									(t (combineAndStrip lstOne lstTwo element))		;; else, combine and strip repetitions
								)
						)
					)
				)
			)
		)
	)
)




(defun combineAndStrip (lstOne lstTwo element)	"Helper function to cmombine two clauses and strip any repeats"
	(removeDuplicates (combine lstOne lstTwo element))	;; remove the duplicated after combining the two clauses
)

(defun removeDuplicates (lst)	"Helper function to remove duplicates from list"
	(cond ((null lst) nil)
		(t	(let ((next (car lst)))
				(cond ((searchForElement next (cdr lst)) (removeDuplicates (cdr lst)))
					(t (cons next (removeDuplicates (cdr lst))))
				)
			)
		)
	)
)


(defun combine (lstOne lstTwo element)	"Helper function to combine two clauses that can be combined through element"
	(cond ((null lstOne) (removeElement lstTwo element))
		(t 	(let ((next (car lstOne)))
				(cond 
					((isNegated next)	;; if next element is negated
						(cond ((equal next (cons 'not (list element))) (combine (cdr lstOne) lstTwo element))	;; if is the negated form of element, remove it 
							(t (cons next (combine (cdr lstOne) lstTwo element)))
						)
					)
					(t 	(cond ((equal next element) (combine (cdr lstOne) lstTwo element))	;; if element is not negated and element is next, remove it 
							(t (cons next (combine (cdr lstOne) lstTwo element)))
						)
					)
				)
		 	)
		)
	)
)

(defun deleteElement (lst element)	"Helper function to delete all instances of an element from a list"
	(cond ((null lst) nil)
		  ((equal element (car lst)) (deleteElement (cdr lst) element))
		(t (cons (car lst) (deleteElement (cdr lst) element)))
	)
)

(defun removeElement (lst element)	"Helper function to remove all instances of an element from a clause, negated or not"
	(cond ((null lst) nil)
		(t 	(let ((next (car lst)))
				(cond 
					((isNegated next)
						(cond ((equal next (cons 'not (list element))) (removeElement (cdr lst) element)) ;; if element is in negated form, remove it
							(t (cons next (removeElement (cdr lst) element)))
						)
					)
					(t 	(cond ((equal next element) (removeElement (cdr lst) element))	;; else, if element is next, remove it
							(t (cons next (removeElement (cdr lst) element)))
						)
					)
				)
		 	)
		)
	)
)

(defun searchForElement (element lst) "Helper function to see if element is in list"
	(cond ((null lst) nil)
		  ((equal (car lst) element) t)
		(t (searchForElement element (cdr lst)))
	)
)

(defun searchForNegatedElement (element lst)	"Helper function to see if negated element is in list"
	(cond ((null lst) nil)
		  ((equal (car lst) (cons 'not (list element))) t)
		(t (searchForNegatedElement element (cdr lst)))
	)
)

(defun isNegated (element)	"Helper function to see if literal is negated"
	(cond ((atom element) nil)
		(t t))
)

(defun getNth (lst index)	"Helper function to get element at index index of list"
	(cond ((zerop index) (car lst))
		(t (getNth (cdr lst) (- index 1)))
	)
)