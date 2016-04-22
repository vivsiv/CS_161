;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp")
  )

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star)
  )

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq star 4)
(setq boxstar 5)
(setq keeperstar 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank)
  )

(defun isWall (v)
  (= v wall)
  )

(defun isBox (v)
  (= v box)
  )

(defun isKeeper (v)
  (= v keeper)
  )

(defun isStar (v)
  (= v star)
  )

(defun isBoxStar (v)
  (= v boxstar)
  )

(defun isKeeperStar (v)
  (= v keeperstar)
  )

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperStar (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun check-row (row)
	(cond
		((null row) t)
		((equal (car row) 2) nil)
		(t (check-row (cdr row)))
	)
)

; (setq p1 '((1 1 1 1 1 1)
; 	   (1 0 3 0 0 1)
; 	   (1 0 2 0 0 1)
; 	   (1 1 0 1 1 1)
; 	   (1 0 0 0 0 1)
; 	   (1 0 0 0 4 1)
; 	   (1 1 1 1 1 1)))
; (format t "(check-row (car p1)) = T? --> ~S~%" (check-row (car p1)))
; (format t "(check-row (third p1)) = nil? --> ~S~%" (check-row (third p1)))

(defun goal-test (s)
	(cond
		((null s) t)
		((not (check-row (car s))) nil)
		(t (goal-test (cdr s)))
	)
);end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;

(defun get-square (S r c)
	(cond
		((< r 0) nil)
		((>= r (length S)) nil)
		((< c 0) nil)
		((>= c (length (car S))) nil)
		(t (car (nthcdr c (car (nthcdr r S)))))
	)
)

; (format t "(get-square p1 0 0) = 1? --> ~S~%" (get-square p1 0 0))
; (format t "(get-square p1 5 4) = 4? --> ~S~%" (get-square p1 5 4))
; (format t "(get-square p1 1 2) = 3? --> ~S~%" (get-square p1 1 2))
; (format t "(getKeeperPosition p1 0) = '(2 1)? --> ~S~%" (getKeeperPosition p1 0))

(defun set-square (S r c v)
	(let (
			(state-top (butlast S (- (length S) r)))
			(state-bottom (nthcdr (+ r 1) S))
			(change-row (car (nthcdr r S)))
		)
		(append 
			state-top 
			(list (append
				(butlast change-row (- (length change-row) c))
				(list v)
				(nthcdr (+ c 1) change-row)
			)) 
			state-bottom
		)
	)
)

; (printState p1)
; (setq p1g (set-square p1 4 3 2))
; (printState p1g)

;Takes a State, A function to apply to the column position, and the Keeper's start position
(defun try-right-left (S Dfun K)
	(let (
			(c-pos (car K))
			(r-pos (cadr K))
			;What the new keepers value will be (depending on if it sits on a goal or not)
			(new-keeper (if (= (get-square S (cadr K) (car K)) 3) 0 4))
		)
		(cond
			;Valid Moves
			;Moves that Need One Space to right or left
			((not (null (get-square S r-pos (funcall Dfun c-pos 1))))
				(cond
					;Blank to right or left
					((= (get-square S r-pos (funcall Dfun c-pos 1)) 0) 
						(set-square 
							(set-square S r-pos c-pos new-keeper) 
							r-pos (funcall Dfun c-pos 1) 3
						)
					)
					;Goal to right or left
					((= (get-square S r-pos (funcall Dfun c-pos 1)) 4) 
						(set-square 
							(set-square S r-pos c-pos new-keeper) 
							r-pos (funcall Dfun c-pos 1) 6
						)
					)
					;Moves that Need 2 Spaces to right or left
					((not (null (get-square S r-pos (funcall Dfun c-pos 2))))
						(cond
							;Box then blank to right or left
							((and
								(= (get-square S r-pos (funcall Dfun c-pos 1)) 2) 
								(= (get-square S r-pos (funcall Dfun c-pos 2)) 0) 
							 )
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										r-pos (funcall Dfun c-pos 1) 3
									) 
									r-pos (funcall Dfun c-pos 2) 2
								)
							)
							;Box&Goal then blank to right or left
							((and
								(= (get-square S r-pos (funcall Dfun c-pos 1)) 5) 
								(= (get-square S r-pos (funcall Dfun c-pos 2)) 0) 
							 )
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										r-pos (funcall Dfun c-pos 1) 6
									) 
									r-pos (funcall Dfun c-pos 2) 2
								)
							)
							;Box then goal to right or left
							((and
								(= (get-square S r-pos (funcall Dfun c-pos 1)) 2) 
								(= (get-square S r-pos (funcall Dfun c-pos 2)) 4) 
							 )
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										r-pos (funcall Dfun c-pos 1) 3
									) 
									r-pos (funcall Dfun c-pos 2) 5
								)
							)	
							;Box&Goal then goal to right or left
							((and
								(= (get-square S r-pos (funcall Dfun c-pos 1)) 5) 
								(= (get-square S r-pos (funcall Dfun c-pos 2)) 4) 
							 )
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										r-pos (funcall Dfun c-pos 1) 6
									) 
									r-pos (funcall Dfun c-pos 2) 5
								)
							)
						)
					)
				)
			)
			;All Invalid Moves
			(t nil)
		)
	)
)

(defun try-up-down (S Dfun K)
	(let (
			(c-pos (car K))
			(r-pos (cadr K))
			;What the new keepers value will be (depending on if it sits on a goal or not)
			(new-keeper (if (= (get-square S (cadr K) (car K)) 3) 0 4))
		)
		(cond
			;Valid Moves
			;Moves that Need One Space above or below
			((not (null (get-square S (funcall Dfun r-pos 1) c-pos)))
				(cond
					;Blank above or below
					((= (get-square S (funcall Dfun r-pos 1) c-pos) 0) 
						(set-square 
							(set-square S r-pos c-pos new-keeper) 
							(funcall Dfun r-pos 1) c-pos 3
						)
					)
					;Goal above or below
					((= (get-square S (funcall Dfun r-pos 1)  c-pos) 4) 
						(set-square 
							(set-square S r-pos c-pos new-keeper) 
							(funcall Dfun r-pos 1) c-pos 6
						)
					)
					((not (null (get-square S (funcall Dfun r-pos 2) c-pos)))
						(cond
							;Box then blank above or below
							((and
								(= (get-square S (funcall Dfun r-pos 1) c-pos) 2) 
								(= (get-square S (funcall Dfun r-pos 2) c-pos) 0) 
							 )
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										(funcall Dfun r-pos 1) c-pos 3
									) 
									(funcall Dfun r-pos 2) c-pos 2
								)
							)
							;Box&Goal then blank above or below
							((and
								(= (get-square S (funcall Dfun r-pos 1) c-pos) 5) 
								(= (get-square S (funcall Dfun r-pos 2) c-pos) 0) 
							 )
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										(funcall Dfun r-pos 1) c-pos 6
									) 
									(funcall Dfun r-pos 2) c-pos 2
								)
							)
							;Box then goal above or below
							((and
								(= (get-square S (funcall Dfun r-pos 1) c-pos) 2) 
								(= (get-square S (funcall Dfun r-pos 2) c-pos) 4) 
							 )
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										(funcall Dfun r-pos 1) c-pos 3
									) 
									(funcall Dfun r-pos 2) c-pos 5
								)
							)	
							;Box&Goal then goal above or below
							((and
								(= (get-square S (funcall Dfun r-pos 1) c-pos) 5) 
								(= (get-square S (funcall Dfun r-pos 2) c-pos) 4) 
							 )
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										(funcall Dfun r-pos 1) c-pos 6
									) 
									(funcall Dfun r-pos 2) c-pos 5
								)
							)
						)
					)
				)
			)
			;All Invalid Moves
			(t nil)
		)
	)
)

(defun try-move (S D)
	(cond
		((equal D 'r) (try-right-left S #'+ (getKeeperPosition S 0)))
		((equal D 'l) (try-right-left S #'- (getKeeperPosition S 0)))
		((equal D 'd) (try-up-down S #'+ (getKeeperPosition S 0)))
		((equal D 'u) (try-up-down S #'- (getKeeperPosition S 0)))
		(t nil)
	)
)


(setq ptest '(
	   (1 1 1 1 1 1 1)
	   (1 0 3 0 0 0 1)
	   (1 0 0 2 0 0 1)
	   (1 0 5 0 2 4 1)
	   (1 0 0 2 0 0 1)
	   (1 0 3 4 0 0 1)
	   (1 1 2 1 1 1 1)
	)
)
(printState ptest)
(setq ptestr (try-move ptest 'r))
(printState ptestr)
(setq ptestl (try-move ptest 'l))
(printState ptestl)
(setq ptestd (try-move ptest 'd))
(printState ptestd)
(setq ptestd (try-move ptest 'u))
(printState ptestd)


(defun next-states (s)
  (let* (
	  	 (pos (getKeeperPosition s 0))
		 (x (car pos))
		 (y (cadr pos))
		 ;x and y are now the coordinate of the keeper in s.
		 (result (append 
		 			(list (try-move s 'r))
		 			(list (try-move s 'l))
		 			(list (try-move s 'd))
		 			(list (try-move s 'u))
		 		)
		 )
	)
    (cleanUpList result);end
  );end let
);

; (setq s1 '((1 1 1 1 1)
;  (1 0 0 4 1)
;  (1 0 2 0 1)
;  (1 0 3 0 1)
;  (1 0 0 0 1)
;  (1 1 1 1 1)
; ))

; (printState s1)
; (printState (next-states s1))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
	0
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;

(defun count-column-boxes (r num-boxes)
	(cond
		((null r) num-boxes)
		((= (car r) 2) (count-column-boxes (cdr r) (+ num-boxes 1)))
		(t (count-column-boxes (cdr r) num-boxes))
	)
)

(defun count-row-boxes (s num-boxes)
	(cond
		((null s) num-boxes)
		(t (count-row-boxes (cdr s) (count-column-boxes (car s) num-boxes)))
	)
)

(defun h1 (s)
	(count-row-boxes s 0)
)

(defun get-elem-columns (row row-num col-num elem elist)
	(cond
		((null row) elist)
		((= (car row) elem)  
			(get-elem-columns (cdr row) row-num (+ col-num 1) elem
				(append elist (list (list row-num col-num)))
			)
		)
		(t (get-elem-columns (cdr row) row-num (+ col-num 1) elem elist))
	)
)

(defun get-elem-rows (s row-num elem)
	(cond
		((null s) nil)
		(t (append
			(get-elem-columns (car s) row-num 0 elem nil) 
			(get-elem-rows (cdr s) (+ row-num 1) elem)
		   ) 
		)
	)
)

(defun dist (e1 e2)
	(let (
			(x1 (car e1))
			(y1 (cadr e1))
			(x2 (car e2))
			(y2 (cadr e2))
		)
		(sqrt 
			(+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))
		)
	)
)

(defun min-dist (box goals min)
	(cond
		((null goals) min)
		((< (dist box (car goals)) min) 
			(min-dist box (cdr goals) (dist box (car goals)))
		)
		(t (min-dist box (cdr goals) min))
	)
)

(setq gbtest '(
	   (1 1 1 1 1 1 1)
	   (1 0 2 0 2 0 1)
	   (1 0 0 2 4 0 1)
	   (1 4 5 6 5 4 1)
	   (1 0 0 2 0 0 1)
	   (1 0 0 4 2 0 1)
	   (1 1 1 1 1 1 1)
	)
)
(format t "getting boxes (get-elem-rows gbtest 0 2) = ~S~%" (get-elem-rows gbtest 0 2))
(format t "getting goals (get-elem-rows gbtest 0 4) = ~S~%" (get-elem-rows gbtest 0 4))

(defun max-min-dist (boxes goals max-bg-dist max)
	(cond
		((null boxes) max)
		((> (min-dist (car boxes) goals max-bg-dist) max)
			(max-min-dist (cdr boxes) goals max-bg-dist (min-dist (car boxes) goals max-bg-dist))
		)
		(t (max-min-dist (cdr boxes) goals max-bg-dist max))
	)
)

;maximum minimum distance from a box to a goal
(defun h2 (s)
	(let (
			(boxes (get-elem-rows s 0 2))
			(goals (get-elem-rows s 0 4))
			(max-bg-dist (* (length s) (length (car s))))
		)
		(floor (max-min-dist boxes goals max-bg-dist 0))
	)
)

(defun sum-min-dists (boxes goals max-bg-dist sum)
	(cond
		((null boxes) sum)
		(t (sum-min-dists 
				(cdr boxes) goals max-bg-dist 
				(+ sum (min-dist (car boxes) goals max-bg-dist))
		   )
		)
	)
)

;sum of minimum distances from boxes to goal
(defun h3 (s)
	(let (
			(boxes (get-elem-rows s 0 2))
			(goals (get-elem-rows s 0 4))
			(max-bg-dist (* (length s) (length (car s))))
		)
		(floor (sum-min-dists boxes goals max-bg-dist 0))
	)
)

; (setq ph1test '((1 1 1 1 1 1)
; 	   (1 0 3 2 2 1)
; 	   (1 0 2 0 0 1)
; 	   (1 1 0 1 1 1)
; 	   (1 0 2 0 2 1)
; 	   (1 5 0 2 4 1)
; 	   (1 1 1 1 1 1)))
; (format t "(h1 ph1test) = 6? --> ~S~%" (h1 ph1test))
; (format t "(check-row (third p1)) = nil? --> ~S~%" (check-row (third p1)))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h303652195 (s)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also privide 2 additional number per problem:
 |    1) # of nodes expanded by A* using our next-states and h0 heuristic.
 |    2) the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below 
 | was solved by 80 nodes expansion of A* and its optimal solution depth is 7.
 | 
 | Your implementation may not result in the same number of nodes expanded, but it should probably
 | give something in the same ballpark. As for the solution depth, any admissible heuristic must 
 | make A* return an optimal solution. So, the depths of the optimal solutions provided could be used
 | for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#

;(80,7)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))

;(110,10)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(211,12)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(300,13)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))

;(551,10)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(722,12)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(1738,50)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(1763,22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))

;(1806,41)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(10082,51)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 0 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(??,25)
(setq p18 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 0 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))
;(??,21)
(setq p19 '((0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 0 1 0 0 1 0 0 0 0)
	    (0 0 0 0 0 0 3 0 0 0 2 0)
	    (0 0 0 0 1 0 0 1 0 0 0 4)
	    (1 1 1 1 0 0 0 0 1 1 1 1)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 0 0 0 1 0 0 0)
	    (0 0 0 1 0 2 0 4 1 0 0 0)))

;(??,??)
(setq p20 '((0 0 0 1 1 1 1 0 0)
	    (1 1 1 1 0 0 1 1 0)
	    (1 0 0 0 2 0 0 1 0)
	    (1 0 0 5 5 5 0 1 0)
	    (1 0 0 4 0 4 0 1 1)
	    (1 1 0 5 0 5 0 0 1)
	    (0 1 1 5 5 5 0 0 1)
	    (0 0 1 0 2 0 1 1 1)
	    (0 0 1 0 3 0 1 0 0)
	    (0 0 1 1 1 1 1 0 0)))

;(??,??)
(setq p21 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 0 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;(??,??)
(setq p22 '((0 0 0 0 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 0 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 0 0 1 2 0 0 1 0 0 0 0 0 0 0 0 0 0)
	    (0 0 1 1 1 0 0 2 1 1 0 0 0 0 0 0 0 0 0)
	    (0 0 1 0 0 2 0 2 0 1 0 0 0 0 0 0 0 0 0)
	    (1 1 1 0 1 0 1 1 0 1 0 0 0 1 1 1 1 1 1)
	    (1 0 0 0 1 0 1 1 0 1 1 1 1 1 0 0 4 4 1)
	    (1 0 2 0 0 2 0 0 0 0 0 0 0 0 0 0 4 4 1)
	    (1 1 1 1 1 0 1 1 1 0 1 3 1 1 0 0 4 4 1)
	    (0 0 0 0 1 0 0 0 0 0 1 1 1 1 1 1 1 1 1)
	    (0 0 0 0 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s star) (format t "."))
	((= s boxstar) (format t "*"))
	((= s keeperstar) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun
