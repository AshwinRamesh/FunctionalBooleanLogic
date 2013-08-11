;;;; Assignment 1 - COMP3109
;;;; @author Ashwin Ramesh


(defun find-vars (x) ; x represents a query
	(if (typep x 'list)
		(case (list-length x)
			((1)
				(cons (find-vars (first x)) ())
			)
			((2)
				(if (and (typep (first x) 'symbol) (typep (first (rest x)) 'symbol))
					(cons (first (rest x)) ())
					(append (find-vars (first x)) (find-vars (first (rest x))))
				)
			)
			(otherwise
				(append (find-vars (first x)) (find-vars (rest x))) ; recursively solve, first item and rest of list
			)
		)
	)
)
