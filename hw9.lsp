
(defconstant VARIABLES '(X Y Z W))

(defconstant CLAUSEPRODS
'(
   ((E X Y) (A (I X Y) (I Y X)))
   ((E X Y Z) (A (E X Z) (E Y Z)))
   ((I X Y) (O (N X) Y))
   ((N (N X)) X)
   ((A X Y Z) (A X (A Y Z)))
   ((O X Y Z) (O X (O Y Z)))
   ((N (A X Y)) (O (N X) (N Y)))
   ((N (O X Y)) (A (N X) (N Y)))
   ((O X (A Y Z)) (A (O Y X) (O Z X)))
   ((O (A Y Z) X) (A (O Y X) (O Z X)))
))

(defun INTERPRET (EXP PRODS)	"Top level interpret function that calls interprethelper function to output final working memory"
	(INTERPRETHELPER EXP PRODS PRODS)
)

(defun INTERPRETHELPER (exp prods leftoverProds)	"Auxiliary function for interpret that keeps track of leftover possible productions"
	(cond ((null leftoverProds) exp)	;; ran out of possible productions, done so return working memory 
		(t	
			(let* 	((currProd (car leftoverProds))	;; next possible production 
				   	(realExp (fireSubexpressions exp (getProductionsUntil currProd prods)))	;; recurse on all subexpressions up to currProd
				   	(matchResult (match realExp (getFirst currProd))))	;; attempt to match the resulting expression to currProd
				   	(cond 
				   		((equal matchResult 'fail) (interprethelper realExp prods (cdr leftoverProds)))	;; if no match, try the remaining productions
						(t (interpret (remodel matchResult (getSecond currProd)) prods))	;; if there is a match, remodel after RHS and start over from first production
				   	)
			)
		)
	)
)

(defun fireSubexpressions (exp prods)	"Auxiliary function to fire all possible subexpressions inside of a top-level expression"
	(cond ((null exp) nil)	;; no more sub expression left 
		  ((atom exp) exp)	;; no subexpressions of a non-list structure 
		(t (cons (INTERPRET (car exp) prods) (fireSubexpressions (cdr exp) prods)))	;; interpret each subexpression and add to the rest 
	)
)

(defun getProductionsUntil (currProd prods)	"Auxiliary function that returns the list of productions above or at the current production"
	(cond ((null prods) nil)
		  ((equal currProd (car prods)) (list (car prods)))	;; this is the last production, so simply return it in a list
		(t (cons (car prods) (getProductionsUntil currProd (cdr prods))))	;; append the current production to the rest 
	)
)

(defun REMODEL (aliases structure)	"Auxiliary function to remodel and plug in aliases for a given structure (RHS Production)"
	(cond ((null structure) nil)	;; if null, no more translations left 
		  ((atom structure) 		;; if structure is an atom, remodeling must be careful 
		  	(cond 
				((isVariable structure VARIABLES) (getAlias structure aliases))	;; if it is a variable, just return the alias 
				(t structure)													;; else, just return the constant 
			)
		  )
		(t							;; structure is a list 
			(let ((nextStructure (car structure)))	
				(cond 
					((atom nextStructure)	;; if next item to fill is an atom
						(cond 
							((isVariable nextStructure VARIABLES)	;; if it is a variable 
								(cons (getAlias nextStructure aliases) (REMODEL aliases (cdr structure)))	;; append the alias to the rest 
							)
							(t 
								(cons nextStructure (REMODEL aliases (cdr structure)))	;; if not a variable, just append the constant 
							)
						)
					)
					(t 		;; next item in list to fill is a list itself, so recurse 
						(cons (REMODEL aliases nextStructure) (REMODEL aliases (cdr structure)))	;; simply recurse and append to rest 
					)
				)
			)
		)
	)
)

(defun MATCH (expression structure)		"Auxiliary function to match an expression to a given structure, returns 'fail on failure or a list of aliases on success"
    (cond 	
		((and (null structure) (null expression)) nil)	;; both finished matching in entirety
		((null structure) 'fail)						;; expression is too long to match 
		((null expression) 'fail)						;; expression is too short to match
		((atom structure) 								;; structure is a single atom 
			(cond 
				((isVariable structure VARIABLES)	;; it is a variable, will match with anything
					(cond 
						((atom expression)			;; if expression is an atom, return its new alias 
							(list structure expression)
						)
						(t							;; expression is a list 
							(cond 
								((equal (length expression) 1) (list structure expression))	;; can only match if list size is 1
								(t 'fail)													;; if more elements in list, no match 
							)
						)
					) 
				)
				(t									;; structure atom is NOT a variable 
					(cond 
						((atom expression)			;; expression is also an atom 
							(cond 
								((equal expression structure) nil)	;; if equal, return nil because not an alias but still match
								(t 'fail)							;; not equal, no match 
							)
						)
						(t 'fail)					;; expression is a list, can't match a list to a non-variable atom 
					)
				)
			)
		) 
		((atom expression) 'fail)	;; expression is an atom, and match structure is a list, could never match 
        (t 	(let	((nextExpression (car expression)) (nextStructure (car structure)))	;; both expression and structure are lists 
				(cond 
					((atom nextStructure) ;; next item to match is an atom 
						(cond 
							((isVariable nextStructure VARIABLES)	;; it is a variable, will match with anything 
								(let ((resultOfRemaining (match (cdr expression) (cdr structure))))	;; try remaining matches
									(cond ((equal resultOfRemaining 'fail) 'fail)					;; if they fail, return fail 
										(t (cons (list nextStructure nextExpression) resultOfRemaining)) ;; else, append alias to return list
									)
								)
							)
							(t			;; next item to match is not a variable, must be explicity matched 
								(cond 
									((equal nextExpression nextStructure)	;; explicit match 
										(let ((resultOfRemaining (match (cdr expression) (cdr structure)))) ;; try remaining matches
											(cond ((equal resultOfRemaining 'fail) 'fail)					;; if they fail, return fail 
												(t resultOfRemaining)	;; else, constant matches -> no alias so no append to list 
											)
										)
									)
									(t 'fail)
								)
							)
						)
					)
					(t 	;; next item to match is a list 
						(cond 
							((atom nextExpression) 'fail)	;; next item of expression is not a list, can't match 
							(t 								;; next item is also a list 
								(let ((matchOfLists (match nextExpression nextStructure)))	;; recursively match the two lists 
									(cond
										((equal matchOfLists 'fail) 'fail)	;; if inner match fails, return fail
										(t 
											(let ((resultOfRemaining (match (cdr expression) (cdr structure)))) ;; try remaining matches
												(cond ((equal resultOfRemaining 'fail) 'fail)					;; if they fail, return fail 
													(t (append matchOfLists resultOfRemaining))	;; else, append found aliases to results 
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

(defun isVariable (item variableList) "Auxiliary function to see if item is inside the variables list"
	(cond ((null variableList) nil)
		  ((equal item (car variableList)) t)
		(t (isVariable item (cdr variableList)))
	)
)


(defun getAlias (key aliases)	"Auxiliary function to get the translation of a variable to its stored value"
	(cond ((null aliases) nil)
		  ((equal key (getFirst (car aliases))) (getSecond (car aliases)))
		(t (getAlias key (cdr aliases)))
	)
)

(defun getFirst (lst)	"Auxiliary function to get first element of a list"
	(car lst)
)

(defun getSecond (lst)	"Auxiliary function to get second element of a list"
	(car (cdr lst))
)
