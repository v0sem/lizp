;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Lab assignment 2: Search
;;    
;;    Data and prototypes
;;    Revised and extended: Simone Santini, 2020/02/27
;;                                          2019/03/07
;;                          Alberto Suarez, 2018/03/23   
;;
;;    Authors: Pablo Sanchez, Antonio Solana
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Problem definition
;;
(defstruct problem
	cities               ; List of cities
	initial-city         ; Initial city
	f-h                  ; reference to a function that evaluates to the 
						 ; value of the heuristic of a state
	f-goal-test          ; reference to a function that determines whether 
						 ; a state fulfils the goal 
	f-search-state-equal ; reference to a predictate that determines whether
						 ; two nodes are equal, in terms of their search state      
	succ)                ; operator (reference to a function) to generate successors
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Node in search tree
;;
(defstruct node 
	city            ; city in which this node places us 
	parent          ; parent node
	action          ; action that generated the current node from its parent
	(depth 0)       ; depth in the search tree
	(g 0)           ; cost of the path from the initial state to this node
	(h 0)           ; value of the heurstic
	(f 0))          ; g + h 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Actions 
;;
(defstruct action
	name              ; Name of the operator that generated the action
	origin            ; City to which the action is applied
	final             ; City in which we are as a result of the application of the action
	cost )            ; Cost of the action
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    Search strategies 
;;
(defstruct strategy
	name              ; name of the search strategy
	node-compare-p)   ; boolean comparison
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    END: Define structures
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;    BEGIN: Define the communication network
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *cities* '(Calais Reims Paris Nancy Orleans 
								St-Malo Brest Nevers Limoges 
								Roenne Lyon Toulouse Avignon Marseille))

(defparameter *trains*
	'((Paris Calais 34.0)     (Calais Paris 34.0)
	(Reims Calais 35.0)       (Calais Reims 35.0)
	(Nancy Reims 35.0)        (Reims Nancy 35.0)
	(Paris Nancy 40.0)        (Nancy Paris 40.0)
	(Paris Nevers 48.0)       (Nevers Paris 48.0)
	(Paris Orleans 23.0)      (Orleans Paris 23.0)
	(Paris St-Malo 40.0)      (St-Malo Paris 40.0)
	(St-Malo Nantes 20.0)     (Nantes St-Malo 20.0)
	(St-Malo Brest 30.0)      (Brest St-Malo 30.0)
	(Nantes Brest 35.0)       (Brest Nantes 35.0)
	(Nantes Orleans 37.0)     (Orleans Nantes 37.0)
	(Nantes Toulouse 80.0)    (Toulouse Nantes 80.0)
	(Orleans Limoges 55.0)    (Limoges Orleans 55.0)
	(Limoges Nevers 42.0)     (Nevers Limoges 42.0)
	(Limoges Toulouse 25.0)   (Toulouse Limoges 25.0)
	(Toulouse Lyon 60.0)      (Lyon Toulouse 60.0)
	(Lyon Roenne 18.0)        (Roenne Lyon  18.0)
	(Lyon Avignon 30.0)       (Avignon Lyon 30.0)
	(Avignon Marseille 16.0)  (Marseille Avignon 16.0)
	(Marseille Toulouse 65.0) (Toulouse Marseille 65.0)))
	

(defparameter *heuristic* 
	'((Calais 0.0) (Reims 25.0) (Paris 30.0) 
	(Nancy 50.0) (Orleans 55.0) (St-Malo 65.0)
	(Nantes 75.0) (Brest 90.0) (Nevers 70.0) 
	(Limoges 100.0) (Roenne 85.0) (Lyon 105.0)
	(Toulouse 130.0) (Avignon 135.0) (Marseille 145.0)))

(defparameter *origin* 'Marseille)

(defparameter *destination* '(Calais))

(defparameter *mandatory* '(Paris))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; BEGIN: Exercise 1 -- Evaluation of the heuristics
;;
;; Returns the value of the heuristics for a given state
;;
;;  Input:
;;    city: the city where we are
;;    sensors: a sensor list, that is a list of pairs
;;                (state time-est)
;;             where the first element is the name of a city and the second
;;             a number estimating the costs to reach the goal
;;
;;  Returns:
;;    The cost (a number) or NIL if the state is not in the sensor list
;;

(defun f-h (city heuristic)
	(second (assoc city heuristic)))


;;
;; END: Exercise 1 -- Evaluation of the heuristic
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 2 -- Navigation operators
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Navigation operator
;;
;;  Returns the actions that can be carried out from the current city
;;
;;  Input:
;;    city:       the city where we are
;;    lst-edges:  list of edges of the graph, each element is of the 
;;                form: (source destination cost1)
;;
;;  Returns
;;    A list of action structures with the origin in the current city and
;;    the destination in the cities to which the current one is connected
;;
(defun navigate (city lst-edges)
	(mapcar #'(lambda (x) (make-action :name 'UselessName :origin (nth 0 x) :final (nth 1 x) :cost (nth 2 x)))
		(remove-if-not #'(lambda (x) (eq (car x) city)) lst-edges))
	)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 3 -- Goal test
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Goal test
;;
;;  Returns T or NIl depending on whether a path leads to a final state
;;
;;  Input:
;;    node:       node structure that contains, in the chain of parent-nodes,
;;                a path starting at the initial state
;;    destinations: list with the names of the destination cities
;;    mandatory:  list with the names of the cities that is mandatory to visit
;;
;;  Returns
;;    T: the path is a valid path to the final state
;;    NIL: invalid path: either the final city is not a destination or some
;;         of the mandatory cities are missing from the path.
;;
(defun f-goal-test (node destination mandatory)
  (if (not (null (member (node-city node) destination)))
      (f-goal-test-aux node mandatory)
      NIL))

(defun f-goal-test-aux (node mandatory)
  (if (equal (node-parent node) NIL)
      (if (null (remove-if #'(lambda (x) (equal (node-city node) x)) mandatory))
          T
          NIL)
      (f-goal-test-aux (node-parent node) (remove-if #'(lambda (x) (equal (node-city node) x)) mandatory))))

;;
;; END: Exercise 3 -- Goal test
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 4 -- Equal predicate for search states
;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Determines if two nodes are equivalent with respect to the solution
;; of the problem: two nodes are equivalent if they represent the same city 
;, and if the path they contain includes the same mandatory cities.
;;  Input:
;;    node-1, node-1: the two nodes that we are comparing, each one
;;                    defining a path through the parent links
;;    mandatory:  list with the names of the cities that is mandatory to visit
;;
;;  Returns
;;    T: the two ndoes are equivalent
;;    NIL: The nodes are not equivalent
;;
(defun f-search-state-equal (node-1 node-2 &optional mandatory)
		(and
			(equal (node-city node-1) (node-city node-2)) ; Check both cities are the same
			(if mandatory ; If there is a mandatory list
				(and (f-goal-test node-1 (list (node-city node-1)) mandatory) (f-goal-test node-2 (list (node-city node-2)) mandatory)) ; Check both nodes
				T))) ; No mandatory list means we are good

;;
;; END: Exercise  -- Equal predicate for search states
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  BEGIN: Exercise 5 -- Define the problem structure
;;
;;
;;  Note that the connectivity of the netowrk is implicit in the
;;  operator: the operator takes a single parameter: a city name, and
;;  returns a list of actions, indicating to which cities one can move
;;  and at which cost. The lists of edges are placed as constants as
;;  the second parameter of the navigate operator. Also note that,
;;  while the navigate function takes as parameter a city name, the
;;  next operator takes as parameter a node, and extract the
;;  node-city (that is, the name of the city) before calling the
;;  navigate function.
;;

(defparameter *travel* 
	(make-problem
		:cities *cities*
		:initial-city *origin*
		:f-h #'f-h
		:f-goal-test #'f-goal-test
		:f-search-state-equal #'f-search-state-equal
		:succ #'navigate))


;;
;;  END: Exercise 5 -- Define the problem structure
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN Exercise 6: Expand node
;;
;; The main function of this section is "expand-node", which receives
;; a node structure (the node to be expanded) and a problem structure.
;; The problem structure has a navigation operator, and we are
;; interested in the states that can be reached using it.
;;
;; So, in the expand-node function, we first call the operator to see what actions are possible.
;;
;; The operator gives us back a list of actions. We iterate on this
;; list of action and, for each one, we call expand-node-action that
;; creates a node structure with the node that can be reached using
;; that action.
;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;;  Creates a list with all the nodes that can be reached from the
;;  current one using the "succ" operator in a given problem
;;
;;  Input:
;;    node:   the node structure from which we start.
;;    problem: the problem structure with the list of operators
;;
;;  Returns:
;;    A list (node_1,...,node_n) of nodes that can be reached from the
;;    given one
;;
(defun expand-node (node problem)
	(mapcar 
		#'(lambda (act) (make-node 
			:city (action-final act)
			:parent node
			:action act
			:depth (+ 1 (node-depth node))
			:g (+ (action-cost act) (node-g node))
			:h (f-h (action-final act) *heuristic*)
			:f (+ (action-cost act) (f-h (action-final act) *heuristic*))))
		(navigate (node-city node) *trains*)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  BEGIN Exercise 7 -- Node list management
;;;  
;;;  Merges two lists of nodes, one of them ordered with respect to a
;;;  given strategy, keeping the result ordered with respect to the
;;;  same strategy.
;;;
;;;  This is the idea: suppose that the ordering is simply the
;;;  ordering of natural numbers. We have a "base" list that is
;;;  already ordered, for example:
;;;      lst1 --> '(3 6 8 10 13 15)
;;;
;;;  and a list that is not necessarily ordered:
;;;
;;;      nord --> '(11 5 9 16)
;;;
;;;  the call (insert-nodes nord lst1 #'<) would produce
;;;
;;;    (3 5 6 8 9 10 11 13 15 16)
;;;
;;;  The functionality is divided in three functions. The first,
;;;  insert-node, inserts a node in a list keeping it ordered. The
;;;  second, insert-nodes, insert the nodes of the non-ordered list
;;;  into the ordered, one by one, so that the two lists are merged.
;;;  The last function, insert-node-strategy is a simple interface that
;;;  receives a strategy, extracts from it the comparison function, 
;;;  and calls insert-nodes


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a list of nodes in an ordered list keeping the result list
;; ordered with respect to the given comparison function
;;
;; Input:
;;    nodes: the (possibly unordered) node list to be inserted in the
;;           other list
;;    lst-nodes: the (ordered) list of nodes in which the given nodes 
;;               are to be inserted
;;    node-compare-p: a function node x node --> 2 that returns T if the 
;;                    first node comes first than the second.
;;
;; Returns:
;;    An ordered list of nodes which includes the nodes of lst-nodes and 
;;    those of the list "nodes@. The list is ordered with respect to the 
;;   criterion node-compare-p.
;; 
(defun insert-nodes (nodes lst-nodes node-compare-p)
	(sort (append nodes lst-nodes) node-compare-p)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Inserts a list of nodes in an ordered list keeping the result list
;; ordered with respect the given strategy
;;
;; Input:
;;    nodes: the (possibly unordered) node list to be inserted in the
;;           other list
;;    lst-nodes: the (ordered) list of nodes in which the given nodes 
;;               are to be inserted
;;    strategy: the strategy that gives the criterion for node
;;              comparison
;;
;; Returns:
;;    An ordered list of nodes which includes the nodes of lst-nodes and 
;;    those of the list "nodes@. The list is ordered with respect to the 
;;    criterion defined in te strategy.
;; 
;; Note:
;;   You will note that this function is just an interface to
;;   insert-nodes: it allows to call using teh strategy as a
;;   parameter; all it does is to "extract" the compare function and
;;   use it to call insert-nodes.
;;
(defun insert-nodes-strategy (nodes lst-nodes strategy)
	(insert-nodes nodes lst-nodes (strategy-node-compare-p strategy))
	)


;;
;;    END: Exercize 7 -- Node list management
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; BEGIN: Exercise 8 -- Definition of the A* strategy
;;
;; A strategy is, basically, a comparison function between nodes to tell 
;; us which nodes should be analyzed first. In the A* strategy, the first 
;; node to be analyzed is the one with the smallest value of g+h
;;
(defparameter *A-star*
	(make-strategy
		:name 'A-star
		:node-compare-p #'(lambda (x y) (< (node-f x) (node-f y)))
	)
)

;;
;; END: Exercise 8 -- Definition of the A* strategy
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 9: Search algorithm
;;;
;;;    Searches a path that solves a given problem using a given search
;;;    strategy. Here too we have two functions: one is a simple
;;;    interface that extracts the relevant information from the
;;;    problem and strategy structure, builds an initial open-nodes
;;;    list (which contains only the starting node defined by the
;;;    state), and initial closed node list (the empty list), and calls
;;;    the auxiliary function.
;;;
;;;    The auxiliary is a recursive function that extracts nodes from
;;;    the open list, expands them, inserts the neighbors in the
;;;    open-list, and the expanded node in the closed list. There is a
;;;    caveat: with this version of the algorithm, a node can be
;;;    inserted in the open list more than once. In this case, if we
;;;    extract a node in the open list and the following two condition old:
;;;
;;;     the node we extract is already in the closed list (it has
;;;     already been expanded)
;;;       and
;;;     the path estimation that we have is better than the one we
;;;     obtain from the node in the open list
;;;
;;;     then we ignore the node.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Interface function for the graph search. 
;;
;;  Input:
;;    problem: the problem structure from which we get the general 
;;             information (goal testing function, action operatos,
;;             starting node, heuristic, etc.
;;    strategy: the strategy that decide which node is the next extracted
;;              from the open-nodes list
;;
;;    Returns:
;;     NIL: no path to the destination nodes
;;     If these is a path, returns the node containing the final state.
;;
;;    See the graph-search-aux for the complete structure of the
;;    returned node. 
;;    This function simply prepares the data for the auxiliary
;;    function: creates an open list with a single node (the source)
;;    and an empty closed list.
;;
(defun exp-cond (node closed)
	(let ((found-node (find node closed :test #'f-search-state-equal)))
		(if (NULL found-node)
			T
			(< (node-g node) (node-g found-node))
		)
	)
)

(defun graph-search-aux (problem strategy open closed goal-test)
	(if (or (NULL open) (NULL (first open)))
		NIL
		(if (funcall goal-test (first open) *destination* *mandatory*)
			(first open) ; Are we there yet?
			(if (exp-cond (first open) closed)
				(graph-search-aux
					problem
					strategy
					(insert-nodes-strategy (expand-node (first open) problem) (rest open) strategy)
					(cons (first open) closed)
					goal-test)
				(graph-search-aux problem strategy (rest open) closed goal-test)
			)
		)
	)
)

(defun graph-search (problem strategy)
	(graph-search-aux
		problem
		strategy
		(list (make-node :city (problem-initial-city problem) :parent NIL :action NIL))
		NIL
		#'f-goal-test)
)

;;
;; END: Exercise 9 -- Search algorithm
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 10: Solution path
;;;
;*** solution-path ***

(defun solution-path (node)
	(if (null (node-parent node))
		(list (node-city node))
		(append (solution-path (node-parent node)) (list (node-city node)))))


;;; 
;;;    END Exercise 10: Solution path / action sequence
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 
;;;    BEGIN Exercise 11: More strategies
;;;

(defparameter *depth-first*
	(make-strategy
		:name 'depth-first
		:node-compare-p #'(lambda (x y) (> (node-depth x) (node-depth y)))
	)
)

(defparameter *breadth-first*
	(make-strategy
		:name 'breadth-first
		:node-compare-p #'(lambda (x y) (< (node-depth x) (node-depth y)))
	)
)

;;; 
;;;    END Exercise 11: More strategies
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
