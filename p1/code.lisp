;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun newton (f df-dx max-iter x0 &optional (tol-abs 0.0001))
  (if  (= max-iter 0) nil
  	(let ((xn (- x0 (/ (funcall f x0) (funcall df-dx x0)))))
    (if (or (< (abs (- x0 xn)) tol-abs))
      xn
      (newton f df-dx (- max-iter 1) xn tol-abs)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun newton-all (f df-dx max-iter seeds &optional (tol-abs 0.0001))
  (mapcar #'(lambda (x0)
    (newton f df-dx max-iter x0 tol-abs)) seeds))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun combine-elt-lst (elt lst)
	(mapcar #'(lambda (x) (list elt x))
        lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-lst-lst (lst1 lst2)
  	(reduce #'append (mapcar #'(lambda(elt) (combine-elt-lst elt lst2)) lst1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun combine-list-of-lsts (lolsts)
	"Combinations of N elements, each of wich


	INPUT:  lstolsts: list of N sublists (list1 ... listN)


	OUTPUT: list of sublists of N elements, such that in each 
					sublist the first element is from list1
								the second element is from list 2
								...
								the Nth element is from list N"
	(if (null lolsts)
		nil
		(if (null (rest lolsts))
			(mapcar #'list (first lolsts))
			(let ((n1 (combine-list-of-lsts (rest lolsts))))
				(apply #'append 
					(mapcar #'(lambda(lst) 
							(mapcar #'(lambda(x) (cons x lst)) (first lolsts))
						) 
						n1
					)
				)
			)
		)
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scalar-product (x y)
	"Calculates the scalar product of two vectors
 
	 INPUT:  x: vector, represented as a list
					 y: vector, represented as a list
 
	 OUTPUT: scalar product between x and y


	 NOTES: 
				* Implemented with mapcar"
		(reduce #'+ (mapcar #'* x y))
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-norm


(defun euclidean-norm (x)
	"Calculates the euclidean (l2) norm of a vector
	 
		INPUT:  x: vector, represented as a list


		OUTPUT: euclidean norm of x"
	(sqrt (scalar-product x x))
)


________________


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-distance


(defun euclidean-distance (x y) 
	"Calculates the euclidean (l2) distance between two vectors
 
		INPUT: x: vector, represented as a list
					 y: vector, represented as a list


		OUTPUT: euclidean distance between x and y"
		(euclidean-norm (mapcar #'- x y))
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun cosine-similarity (x y) 
	"Calculates the cosine similarity between two vectors


		INPUT:  x: vector, representad as a list
						y: vector, representad as a list


		OUTPUT: cosine similarity between x and y


		NOTES: 
			 * Evaluates to NIL (not defined)
				 if at least one of the vectors has zero norm.
			 * The two vectors are assumed to have the same length"
		(let ((xn (euclidean-norm x)) (yn (euclidean-norm y)))
			(if (or (= 0 xn) (= 0 yn)) nil
				(/ (scalar-product x y) (* xn yn))))
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun angular-distance (x y) 
	"Calculates the angular distance between two vectors


	 INPUT:  x: vector, representad as a list
					 y: vector, representad as a list


	 OUTPUT: cosine similarity between x and y


	 NOTES: 
			* Evaluates to NIL (not well defined)
				if at least one of the vectors has zero norm.
			* The two vectors are assumed to have the same length"

		(let ((var (cosine-similarity x y)))
			(if (null var) nil
			(/ (acos var) pi)))
	)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; select-vectors

(defun similarity-cons (vector1 vector2)
    (cons vector1 (cosine-similarity vector1 vector2)))

(defun sim-map (lst-vectors vector)
    (if (null lst-vectors)
        ()
        (cons
            (cons (first lst-vectors) (cosine-similarity (first lst-vectors) vector))

(defun select-vectors (lst-vectors test-vector similarity-fn &optional (threshold 0))
		"Selects from a list the vectors whose similarity to a 
		 test vector is above a specified threshold. 
		 The resulting list is ordered according to this similarity.
 
		 INPUT:  lst-vectors:   list of vectors
						 test-vector:   test vector, representad as a list
						 similarity-fn: reference to a similarity function
						 threshold:     similarity threshold (default 0)
			
		 OUTPUT: list of pairs. Each pair is a list with
						 a vector and a similarity score.
						 The vectors are such that their similarity to the 
						 test vector is above the specified threshold.
						 The list is ordered from larger to smaller 
						 values of the similarity score 
		 
		 NOTES: 
				* Uses remove-if and sort"
	(defun check (x)
		(< (rest x) threshold))

	(defun sort-fn (x y)
		(> (rest x) (rest y)))

	(sort (remove-if #'check (sim-map lst-vectors test-vector)) #'sort-fn)
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


________________


(defun nearest-neighbor (lst-vectors test-vector distance-fn)
	"Selects from a list the vector that is closest to the 
	 reference vector according to the specified distance function 
 
	 INPUT:  lst-vectors:   list of vectors
					 ref-vector:    reference vector, represented as a list
					 distance-fn:   reference to a distance function
			
	 OUTPUT: List formed by two elements:
					 (1) the vector that is closest to the reference vector 
							 according to the specified distance function
					 (2) The corresponding distance value.


	 NOTES: 
			* The implementation is recursive
			* It ignores the vectors in lst-vectors for which the 
				distance value cannot be computed."
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun backward-chaining (goal lst-rules)
	"Backward-chaining algorithm for propositional logic
 
	 INPUT: goal:      symbol that represents the goal
					lst-rules: list of pairs of the form 
										 (<antecedent>  <consequent>)
										 where <antecedent> is a list of symbols
										 and  <consequent> is a symbol


	 OUTPUT: T (goal derived) or NIL (goal cannot be derived)


	 NOTES: 
				* Implemented with some, every" 


	(backward-chaining-aux goal lst-rules NIL))










________________


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
	(if (null queue) 
			NIL
		(let* ((path (first queue))
					 (node (first path)))
			(if (eql node end) 
					(reverse path)
				(bfs end 
						 (append (rest queue) 
										 (new-paths path node net)) 
						 net))))) 


 (defun new-paths (path node net)
	(mapcar #'(lambda(n) 
				(cons n path)) 
								(rest (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun shortest-path (start end net)
	(bfs end (list (list start)) net))    


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
 
(defun bfs-improved (end queue net)
	)




(defun shortest-path-improved (end queue net)
	)
