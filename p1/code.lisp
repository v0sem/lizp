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

(defun sim-map (lst-vectors vector fun)
    (if (null lst-vectors)
        ()
        (cons
            (cons (first lst-vectors) (funcall fun (first lst-vectors) vector))
            (sim-map (rest lst-vectors) vector fun)
		)
	)
)


(defun select-vectors (lst-vectors test-vector similarity-fn &optional (threshold 0))
    (sort 
		(remove-if #'(lambda(x) (< (rest x) threshold)) (sim-map lst-vectors test-vector similarity-fn)) 
		#'(lambda(x y) (> (rest x) (rest y)))
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lowest-aux (map-lst-vector lowest)
    (if (null map-lst-vector)
        lowest
        (let (
            (last-lowest (cdr lowest))
            (new-lowest (cdr (first map-lst-vector)))
            (new-lowest-cmp (first map-lst-vector)))
            (if (< last-lowest new-lowest)
                (lowest-aux (rest map-lst-vector) lowest)
                (lowest-aux (rest map-lst-vector) new-lowest-cmp))
        )
    )
)

(defun get-lowest (map-lst-vector)
    (lowest-aux map-lst-vector (cons '(0 0 0) 2.0)))


(defun nearest-neighbor (lst-vectors test-vector distance-fn)
    (get-lowest (sim-map lst-vectors test-vector distance-fn)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun backward-chaining-aux (goal lst-rules pending-goals)
	(if (not (null (member goal pending-goals)))
		NIL
		(if (some #'(lambda(x) (and (equal (nth 1 x) goal) (null (first x)))) 
				lst-rules
			)
			T
			(let ((rules (remove-if #'(lambda(x) (not (equal (nth 1 x) goal))) lst-rules)))
				(if (null rules)
					NIL
					(if (some
							#'(lambda(rule) (equal T (every #'(lambda(x) 
								(equal (backward-chaining-aux x (remove-if #'(lambda(x) (equal x rule)) lst-rules) (append pending-goals (list goal))) 'T)) 
								(first rule)
							)))
							rules
						)
						T
						NIL
					)
				)
			)
		)
	)
)


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
