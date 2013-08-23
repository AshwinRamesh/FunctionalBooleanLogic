;;;; Assignment 1 - COMP3109
;;;; @author Ashwin Ramesh


;;; ADDITIONAL FUNCTIONS (these will help solve one or more of the assignment functions)
;; Based of --> http://stackoverflow.com/questions/15760966/lisp-how-can-i-test-if-two-lists-have-the-same-elements
(defun ismember (x liste)
   (cond
      ((null liste) ())
      ((equal (car liste) x) liste)
      (t (ismember x (cdr liste)))))

(defun inclus (liste1 liste2)
   (cond
      ((null liste1) t)
      ((ismember (car liste1) liste2)(inclus (cdr liste1) liste2))
      (t ())))

(defun compare (liste1 liste2)
   (if (and (inclus liste1 liste2) (inclus liste2 liste1))
      t ; lists are the same
      nil ; list are not the same
      ))


;;; END OF ADDITIONAL FUNCTIONS



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
    (cond
        ((and (equal (first query) 's-not) (equal (first (second query)) 's-not)) (simplify (second (second query)))) ;; check for double negatives
        ((and (equal (first query) 's-not) (equal (first (second query)) 's-nand)) (list 's-and (simplify (second (second query))) (simplify (third (second query))))) ;; check for not encapsulating a s-nand (s-not (s-nand A B))
        ((and (equal (first query) 's-nand) (compare (second query) (third query))) (list 'nil)) ;; check if two params of s-nand are the same --> RETURNS NIL
        ((equal (first query) 's-not) (list 's-not (simplify (second query)))) ;; base case 1 (for s-not)
        ((equal (first query) 's-nand) (list 's-nand (simplify (second query)) (simplify (third query)))) ;; base case 2 (for s-nand)
        (t query))) ;; base case 3 return the query since it cannot be simplified further
