(defun refute (lst)
	(refuteHelper lst 0 1 nil)
)


(defun refuteHelper(lst indexOne indexTwo latestResolution)	"Helper function for refuting a sequence of clauses" 
	(cond ((>= indexOne (length lst)) 'fail)
		  ((>= indexTwo (length lst)) (refuteHelper lst (+ indexOne 1) (+ indexOne 2) latestResolution))
		(t 	(let* ((first (getNth lst indexOne))
				   (second (getNth lst indexTwo))
				   (resolution (resolve first second)))
				(write-line "First")
				(write first)
				(write-line "")
				(write-line "Second")
				(write second)
				(write-line "")
				(write-line "Resolution")
				(write resolution)
				(write-line "")
				(write-line "Latest res")
				(write latestResolution)
				(write-line "")
				(cond ((equal resolution 'fail) (refuteHelper lst indexOne (+ indexTwo 1) latestResolution))
					  ((null resolution) 
						(cond ((null latestResolution) (list nil (list first) (list second)))
							  ((equal first (car latestResolution)) (list nil (list second) (cons first (cdr latestResolution))))
							  ((equal second (car latestResolution)) (list nil (list first) (cons second (cdr latestResolution))))
							(t (list nil (list first) (list second)))
						)
					  )
					(t 	(let ((newLst (cons resolution (deleteElement (deleteElement lst first) second))))
							(cond ((equal first (car latestResolution))
									(let ((res (refuteHelper newLst 0 1 (list resolution latestResolution (list second)))))
										(cond ((equal res 'fail) (refuteHelper lst (+ indexOne 1) (+ indexOne 2) latestResolution))
											(t res)
										)
									))
								  ((equal second (car latestResolution))
								  	(let ((res (refuteHelper newLst 0 1 (list resolution (list first) latestResolution))))
										(cond ((equal res 'fail) (refuteHelper lst (+ indexOne 1) (+ indexOne 2) latestResolution))
											(t res)
										)
									))
								(t (let ((res (refuteHelper newLst 0 1 (list resolution (list first) (list second)))))
										(cond ((equal res 'fail) (refuteHelper lst (+ indexOne 1) (+ indexOne 2) latestResolution))
											(t res)
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