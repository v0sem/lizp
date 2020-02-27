;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun newton (f df-dx max-iter x0 &optional (tol-abs 0.0001))
  (if  (= max-iter 0) nil
  	(let ((xn (- x0 (/ (funcall f x0) (funcall df-dx x0)))))
    (if (or (< (abs (- x0 xn)) tol-abs))
      xn
      (newton f df-dx (- max-iter 1) xn tol-abs)))))

(assert (= (newton #'sin #'cos 50 1.0) 0.0))
(assert (= (newton #'sin #'cos 50 2.0) 3.1415927))
(assert (= (newton #'sin #'cos 50 3.0) 3.1415927))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun newton-all (f df-dx max-iter seeds &optional (tol-abs 0.0001))
  (mapcar #'(lambda (x0)
    (newton f df-dx max-iter x0 tol-abs)) seeds))

(assert
	(equal
		(newton-all #'sin #'cos 50 (mapcar #'eval '(1.0 2.0 4.0 6.0)))
		'(0.0 3.1415927 3.1415927 6.2831855)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun combine-elt-lst (elt lst)
	(mapcar #'(lambda (x) (list elt x))
        lst))

(assert
	(equal
		(combine-elt-lst 'a '(1 2 3))
		'((A 1) (A 2) (A 3))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-lst-lst (lst1 lst2)
  	(reduce #'append (mapcar #'(lambda(elt) (combine-elt-lst elt lst2)) lst1)))

(assert
	(equal
		(combine-lst-lst '(a b c) '(1 2))
		'((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun combine-list-of-lsts (lolsts)
	(if (null lolsts)
		nil
		(if (null (rest lolsts))
			(mapcar #'list (first lolsts))
			(let ((n1 (combine-list-of-lsts (rest lolsts))))
				(apply #'append 
					(mapcar #'(lambda(lst) 
							(mapcar #'(lambda(x) (cons x lst)) (first lolsts))
						) 
						n1))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun scalar-product (x y)
		(reduce #'+ (mapcar #'* x y)))

(assert (=
	(scalar-product'(1 2 3) '(3 -1 5))
	16))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-norm


(defun euclidean-norm (x)
	(sqrt (scalar-product x x)))

(assert
	(=
		(euclidean-norm '(3 -1 5))
		5.91608))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; euclidean-distance

(defun euclidean-distance (x y) 
		(euclidean-norm (mapcar #'- x y)))

(assert
	(=
		(euclidean-distance '(1 2 3) '(3 -1 5))
		4.1231055))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cosine-similarity (x y)
		(let ((xn (euclidean-norm x)) (yn (euclidean-norm y)))
			(if (or (= 0 xn) (= 0 yn)) nil
				(/ (scalar-product x y) (* xn yn)))))

(assert (=
	(cosine-similarity '(1 2 3) '(-2 1 3))
	0.6428571))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun angular-distance (x y)
		(let ((var (cosine-similarity x y)))
			(if (null var) nil
			(/ (acos var) pi))))
			
(assert (= 
	(angular-distance '(1 2 3) '(-2 1 3))
	0.2777489))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; select-vectors

(defun sim-map (lst-vectors vector fun)
    (if (null lst-vectors)
        ()
        (cons
            (cons (first lst-vectors) (funcall fun (first lst-vectors) vector))
            (sim-map (rest lst-vectors) vector fun))))

(defun select-vectors (lst-vectors test-vector similarity-fn &optional (threshold 0))
    (sort 
		(remove-if #'(lambda(x) (< (rest x) threshold)) (sim-map lst-vectors test-vector similarity-fn)) 
		#'(lambda(x y) (> (rest x) (rest y)))))

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
                (lowest-aux (rest map-lst-vector) new-lowest-cmp)))))

(defun get-lowest (map-lst-vector)
    (lowest-aux map-lst-vector (cons '(0 0 0) 2.0)))

(defun nearest-neighbor (lst-vectors test-vector distance-fn)
    (get-lowest (sim-map lst-vectors test-vector distance-fn)))

(assert (equal 
	(nearest-neighbor '((-1 -1 -1) (-2 2 2) (-1 -1 1)) '(1 1 1) #'angular-distance)
	(cons '(-2 2 2) 0.39182654)))
