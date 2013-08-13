;;;; Assignment 1 - COMP3109
;;;; @author Ashwin Ramesh

;; Function to find all unique variable names in a query
(defun find-vars (x) ; x represents a query
	(if (typep x 'list)
		(case (list-length x)
			((1)
				(cons (find-vars (first x)) ()) ; list size is 1, thus it can be a list or a operator --> s-var, s-and, s-not
			)
			((2)
				(if (and (typep (first x) 'symbol) (typep (first (rest x)) 'symbol)) ; check to see if both items in list are symbols i.e. (s-var a)
					(cons (first (rest x)) ()) ; return just the variable name in a list
					(union (find-vars (first x)) (find-vars (first (rest x)))) ; recursively call each half of the query
				)
			)
			(otherwise
				(union (find-vars (first x)) (find-vars (rest x))) ; recursively solve, first item and rest of list
			)
		)
	)
)

