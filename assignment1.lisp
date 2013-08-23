;;;; Assignment 1 - COMP3109
;;;; @author Ashwin Ramesh

;; Function to find all unique variable names in a query
(defun find-vars (query) ; query represents a query-
	(if (typep query 'list)
		(case (list-length query)
			((1)
				(cons (find-vars (first query)) ())) ; list size is 1, thus it can be a list or a operator --> s-var, s-and, s-not
			((2)
				(if (and (typep (first query) 'symbol) (typep (first (rest query)) 'symbol)) ; check to see if both items in list are symbols i.e. (s-var a)
					(cons (first (rest query)) ()) ; return just the variable name in a list
					(union (find-vars (first query)) (find-vars (first (rest query)))))) ; recursively call each half of the query
			(otherwise
				(union (find-vars (first query)) (find-vars (rest query))))))) ; recursively solve, first item and rest of list


;; Function to find the outcome of a query
(defun transformer (query)
  (lambda (env)
  	(cond ((equal 's-var (first query)) (second (assoc (second query) env))) ;; check if s-var --> return the value for the var from the env
          ((equal 's-not (first query)) (not (funcall (transformer (second query)) env))) ;; check if s-not --> return opposite of value for var from env
		  ((equal 's-nand (first query)) (not (and (funcall (transformer (second query)) env) (funcall (transformer (third query)) env))))))) ;; check if s-nand --> return a composite recursive call to transformer

;; Function to simplify the query
;; (s-not (s-not (s-var a))) --> (s-var a)
(defun simplify (query)
	(cond ((and (equal (first query) (first (second query))) (equal 's-not (first query))) ;; a condition to check if the first item in the query
                                                                                           ;; and the first item in the rest of the query is s-not
		   (second (second query)))))
