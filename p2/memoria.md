---
title:
- Memoria P2 IA - Búsqueda
subtitle:
- Grupo 2363
author:
- Antonio Solana Vera y Pablo Sánchez Redondo
date:
- 27/02/2020
papersize:
- a4
fontsize:
- 12pt
geometry:
- margin=1in
header-includes:
- \setlength\parindent{0pt}
- \usepackage[spanish]{babel}
---

\maketitle
\thispagestyle{empty}
\clearpage
\tableofcontents
\pagenumbering{roman}
\clearpage
\pagenumbering{arabic}
\setcounter{page}{1}

# Entorno de compilación

Hemos utilizado Emacs (portacle) para compilar y probar nuestras funciones. 

# Ejercicios

## Ejercicio 1

Esta fución simplemente devuelve el valor asignado a cada una de las ciudades, para esto usamos el assoc que busca la tupla coincidente.

```lisp
(defun f-h (city heuristic)
	(second (assoc city heuristic)))
```

## Ejercicio 2

Primero elimina todos los elementos que no tengan la ciudad y luego crea todas las acciones posibles desde ella.

```lisp
(defun navigate (city lst-edges)
	(mapcar #'(lambda (x) (make-action :name 'UselessName :origin (nth 0 x) :final (nth 1 x) :cost (nth 2 x)))
		(remove-if-not #'(lambda (x) (eq (car x) city)) lst-edges)))
```

## Ejercicio 3

En la función principal comprobamos que la ciudad es una de las ciudades de destino, y luego, si es así, llamamos a la función recursiva auxiliar. Ésta comprueba que no es el principio (que no tiene padre), elimina la ciudad de la lista de mandatory si estuviera y vuelve a llamar a la función.

La condición de salida (no hay padre), comprueba después que mandatory está vacío y si lo esta devuelve T y si no NIL.

```lisp
(defun f-goal-test (node destination mandatory)
  (if (not (null (member (node-city node) destination)))
      (f-goal-test-aux node mandatory)
      NIL))

(defun f-goal-test-aux (node mandatory)
  (if (equal (node-parent node) NIL)
      (if (null mandatory)
          T
          NIL)
      (f-goal-test-aux (node-parent node) (remove-if 
      #'(lambda (x) (equal (node-city node) x)) mandatory))))
```

## Ejercicio 4

```lisp
(defun f-search-state-equal (node-1 node-2 &optional mandatory)
	(let ((c1 (node-city node-1)) (c2 (node-city node-1)))
		(and
			(eq c1 c2))
			(if (mandatory)
				(and (f-goal-test node-1 c1 mandatory) (f-goal-test node-2 c2 mandatory))
				T)))
```

## Ejercicio 5

```lisp
(defparameter *travel* 
	(make-problem
		:cities *cities*
		:initial-city *origin*
		:f-h #'f-h
		:f-goal-test #'f-goal-test
		:f-search-state-equal #'f-search-state-equal
		:succ #'navigate))
```

## Ejercicio 6

```lisp
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
```

## Ejercicio 7

```lisp
(defun insert-nodes (nodes lst-nodes node-compare-p)
	(sort (append nodes lst-nodes) node-compare-p))

(defun insert-nodes-strategy (nodes lst-nodes strategy)
	(insert-nodes nodes lst-nodes (strategy-node-compare-p strategy)))
```

## Ejercicio 8

```lisp
(defparameter *A-star*
	(make-strategy
		:name 'A-star
		:node-compare-p #'(lambda (x y) (< (node-f x) (node-f y))))
	)
```