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


;Helper for goal-test, takes a ROW as an argument and checks if it has any boxes
(defun check-row (row)
	(cond
		((null row) t)
		((equal (car row) 2) nil)
		(t (check-row (cdr row)))
	)
)
; check-row tests
; (setq p1 '((1 1 1 1 1 1)
; 	   (1 0 3 0 0 1)
; 	   (1 0 2 0 0 1)
; 	   (1 1 0 1 1 1)
; 	   (1 0 0 0 0 1)
; 	   (1 0 0 0 4 1)
; 	   (1 1 1 1 1 1)))
; (format t "(check-row (car p1)) = T? --> ~S~%" (check-row (car p1)))
; (format t "(check-row (third p1)) = nil? --> ~S~%" (check-row (third p1)))

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of a Sokoban game.
; (no box is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
;Determines if a state is the goal state by checking for any boxes not on goals
(defun goal-test (s)
	(cond
		((null s) t)
		;if a row has a box it is not the goal state
		((not (check-row (car s))) nil)
		(t (goal-test (cdr s)))
	)
);end defun


;next-states helper that takes a STATE, a ROW NUMBER, and a COLUMN NUMBER
;and returns the element at that square
(defun get-square (S r c)
	(cond
		;if check if the square requested is in the board
		((< r 0) nil)
		((>= r (length S)) nil)
		((< c 0) nil)
		((>= c (length (car S))) nil)
		;if it is a valid square get the row, then the element from the row
		(t (car (nthcdr c (car (nthcdr r S)))))
	)
)
; get-square tests
; (format t "(get-square p1 0 0) = 1? --> ~S~%" (get-square p1 0 0))
; (format t "(get-square p1 5 4) = 4? --> ~S~%" (get-square p1 5 4))
; (format t "(get-square p1 1 2) = 3? --> ~S~%" (get-square p1 1 2))
; (format t "(getKeeperPosition p1 0) = '(2 1)? --> ~S~%" (getKeeperPosition p1 0))

;next-states helper that takes a STATE, a ROW NUMBER, a COLUMN NUMBER, and a VALUE
;returns a new state 'object' with the requested square changed to VALUE
(defun set-square (S r c v)
	(let (
			;Top n rows of the state
			(state-top (butlast S (- (length S) r)))
			;Bottom n-r rows of the state
			(state-bottom (nthcdr (+ r 1) S))
			;row to change
			(change-row (car (nthcdr r S)))
		)
		;Rebuild the state
		(append 
			state-top 
			;Rebuild the changed row
			(list 
				(append
					;Append the beginning n columns
					(butlast change-row (- (length change-row) c))
					;change the requested VALUE
					(list v)
					;Append the end n-c columns
					(nthcdr (+ c 1) change-row)
				)
			) 
			state-bottom
		)
	)
)
; set square tests
; (printState p1)
; (setq p1g (set-square p1 4 3 2))
; (printState p1g)

;next-states helper to return the resulting states from moving to the left or right
;Takes a STATE, A FUNCTION to apply to the column position, and the KEEPER'S POSITION
(defun try-right-left (S Dfun K)
	(let (
			(c-pos (car K))
			(r-pos (cadr K))
			;What the new keepers value will be (depending on if it sits on a goal or not)
			(new-keeper (if (= (get-square S (cadr K) (car K)) keeper) blank star))
		)
		(cond
			;Valid Moves
			;Moves that Need One Space to right or left
			((not (null (get-square S r-pos (funcall Dfun c-pos 1))))
				(cond
					;Blank to right or left
					((= (get-square S r-pos (funcall Dfun c-pos 1)) blank) 
						;Return a state with the keeper's value and the blank's value modified
						(set-square 
							(set-square S r-pos c-pos new-keeper) 
							r-pos (funcall Dfun c-pos 1) keeper
						)
					)
					;Goal to right or left
					((= (get-square S r-pos (funcall Dfun c-pos 1)) star)
						;Return a state with the keeper's value and the goal's value modified 
						(set-square 
							(set-square S r-pos c-pos new-keeper) 
							r-pos (funcall Dfun c-pos 1) keeperstar
						)
					)
					;Moves that Need 2 Spaces to right or left
					((not (null (get-square S r-pos (funcall Dfun c-pos 2))))
						(cond
							;Box then blank to right or left
							((and
								(= (get-square S r-pos (funcall Dfun c-pos 1)) box) 
								(= (get-square S r-pos (funcall Dfun c-pos 2)) blank) 
							 )
								;Return a state with the keeper's, box's, and blanks values modified
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										r-pos (funcall Dfun c-pos 1) keeper
									) 
									r-pos (funcall Dfun c-pos 2) box
								)
							)
							;Box&Goal then blank to right or left
							((and
								(= (get-square S r-pos (funcall Dfun c-pos 1)) boxstar) 
								(= (get-square S r-pos (funcall Dfun c-pos 2)) blank) 
							 )
								;Return a state with the keeper's, box & goal's, and blanks's values modified
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										r-pos (funcall Dfun c-pos 1) keeperstar
									) 
									r-pos (funcall Dfun c-pos 2) box
								)
							)
							;Box then goal to right or left
							((and
								(= (get-square S r-pos (funcall Dfun c-pos 1)) box) 
								(= (get-square S r-pos (funcall Dfun c-pos 2)) star) 
							 )
								;Return a state with the keeper's, box's, and blanks values modified
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										r-pos (funcall Dfun c-pos 1) keeper
									) 
									r-pos (funcall Dfun c-pos 2) boxstar
								)
							)	
							;Box&Goal then goal to right or left
							((and
								(= (get-square S r-pos (funcall Dfun c-pos 1)) boxstar) 
								(= (get-square S r-pos (funcall Dfun c-pos 2)) star) 
							 )
								;Return a state with the keeper's, box & goal's, and goal's values modified
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										r-pos (funcall Dfun c-pos 1) keeperstar
									) 
									r-pos (funcall Dfun c-pos 2) boxstar
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

;next-states helper to return the resulting states from moving up or down
;Takes a STATE, A FUNCTION to apply to the column position, and the KEEPER'S POSITION
(defun try-up-down (S Dfun K)
	(let (
			(c-pos (car K))
			(r-pos (cadr K))
			;What the new keepers value will be (depending on if it sits on a goal or not)
			(new-keeper (if (= (get-square S (cadr K) (car K)) keeper) blank star))
		)
		(cond
			;Valid Moves
			;Moves that Need One Space above or below
			((not (null (get-square S (funcall Dfun r-pos 1) c-pos)))
				(cond
					;Blank above or below
					((= (get-square S (funcall Dfun r-pos 1) c-pos) blank) 
						;Return a state with the keeper's value and the blank's value modified
						(set-square 
							(set-square S r-pos c-pos new-keeper) 
							(funcall Dfun r-pos 1) c-pos keeper
						)
					)
					;Goal above or below
					((= (get-square S (funcall Dfun r-pos 1)  c-pos) star) 
						;Return a state with the keeper's value and the goal's value modified
						(set-square 
							(set-square S r-pos c-pos new-keeper) 
							(funcall Dfun r-pos 1) c-pos keeperstar
						)
					)
					((not (null (get-square S (funcall Dfun r-pos 2) c-pos)))
						(cond
							;Box then blank above or below
							((and
								(= (get-square S (funcall Dfun r-pos 1) c-pos) box) 
								(= (get-square S (funcall Dfun r-pos 2) c-pos) blank) 
							 )
								;Return a state with the keeper's, box's, and blank's values modified
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										(funcall Dfun r-pos 1) c-pos keeper
									) 
									(funcall Dfun r-pos 2) c-pos box
								)
							)
							;Box&Goal then blank above or below
							((and
								(= (get-square S (funcall Dfun r-pos 1) c-pos) boxstar) 
								(= (get-square S (funcall Dfun r-pos 2) c-pos) blank) 
							 )
								;Return a state with the keeper's, box & goal's, and blanks values modified
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										(funcall Dfun r-pos 1) c-pos keeperstar
									) 
									(funcall Dfun r-pos 2) c-pos 2
								)
							)
							;Box then goal above or below
							((and
								(= (get-square S (funcall Dfun r-pos 1) c-pos) box) 
								(= (get-square S (funcall Dfun r-pos 2) c-pos) star) 
							 )
								;Return a state with the keeper's, box's, and goal's values modified
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										(funcall Dfun r-pos 1) c-pos keeper
									) 
									(funcall Dfun r-pos 2) c-pos boxstar
								)
							)	
							;Box&Goal then goal above or below
							((and
								(= (get-square S (funcall Dfun r-pos 1) c-pos) boxstar) 
								(= (get-square S (funcall Dfun r-pos 2) c-pos) star) 
							 )
								;Return a state with the keeper's, box&goals's, and goal's values modified
								(set-square 
									(set-square 
										(set-square S r-pos c-pos new-keeper)
										(funcall Dfun r-pos 1) c-pos keeperstar
									) 
									(funcall Dfun r-pos 2) c-pos boxstar
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

;next-states helper that returns the resulting state from trying a move in a Direction
;takes the input STATE and the DIRECTION to try ('r 'l 'd 'u) 
(defun try-move (S D)
	;Applies the appropriate helper based on the input direction
	(cond
		((equal D 'r) (try-right-left S #'+ (getKeeperPosition S 0)))
		((equal D 'l) (try-right-left S #'- (getKeeperPosition S 0)))
		((equal D 'd) (try-up-down S #'+ (getKeeperPosition S 0)))
		((equal D 'u) (try-up-down S #'- (getKeeperPosition S 0)))
		(t nil)
	)
)

;try-move tests
; (setq ptest '(
; 	   (1 1 1 1 1 1 1)
; 	   (1 0 3 0 0 0 1)
; 	   (1 0 0 2 0 0 1)
; 	   (1 0 5 0 2 4 1)
; 	   (1 0 0 2 0 0 1)
; 	   (1 0 3 4 0 0 1)
; 	   (1 1 2 1 1 1 1)
; 	)
; )
; (printState ptest)
; (setq ptestr (try-move ptest 'r))
; (printState ptestr)
; (setq ptestl (try-move ptest 'l))
; (printState ptestl)
; (setq ptestd (try-move ptest 'd))
; (printState ptestd)
; (setq ptestd (try-move ptest 'u))
; (printState ptestd)


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
;Takes a STATE and returns a list of possible legal resulting states from moving in any direction
(defun next-states (s)
  (let* (
	  	 (pos (getKeeperPosition s 0))
		 (x (car pos))
		 (y (cadr pos))
		 ;x and y are now the coordinate of the keeper in s.
		 ; List of results from trying moves in all directions
		 (result (append 
		 			(list (try-move s 'r))
		 			(list (try-move s 'l))
		 			(list (try-move s 'd))
		 			(list (try-move s 'u))
		 		)
		 )
	)
  	;remove any nil's in the list from illegal moves
    (cleanUpList result);end
  );end let
);

; next-states tests
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

;h1 helper that counts the number of boxes in a row
;Takes a ROW and an accumulator for the NUMBER OF BOXES
(defun count-row-boxes (r num-boxes)
	(cond
		((null r) num-boxes)
		;if a box add one to the accumulator
		((= (car r) box) (count-row-boxes (cdr r) (+ num-boxes 1)))
		(t (count-row-boxes (cdr r) num-boxes))
	)
)

;h1 helper that counts the number of boxes in a state
;Takes a STATE and an accumulator for the NUMBER OF BOXES
(defun count-boxes (s num-boxes)
	(cond
		((null s) num-boxes)
		;Use count-row-boxes to accumulate a row.
		(t (count-boxes (cdr s) (count-row-boxes (car s) num-boxes)))
	)
)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; 
;Heuristic function that takes a STATE and counts the number of boxes not on goals
;This heursitic is admissible because the number of boxes not on goals can never over estimate
;the number of moves left to finishe the game (each box not on a goal must be moved on to a goal)
(defun h1 (s)
	(count-boxes s 0)
)

;Heuristic helper gets the coordinates of all instances of an element in a row
;Takes a ROW, the current ROW NUMBER, the current COLUMN NUMBER, the ELEMENT to find, 
;and the LIST OF ELEMENT coordinates
(defun get-elem-pos-rows (row row-num col-num elem elist)
	(cond
		((null row) elist)
		((= (car row) elem) 
			;if you find an element append its location to the element list 
			(get-elem-pos-rows (cdr row) row-num (+ col-num 1) elem
				(append elist (list (list row-num col-num)))
			)
		)
		(t (get-elem-pos-rows (cdr row) row-num (+ col-num 1) elem elist))
	)
)

;Heuristic helper creates a list of the coordinates of all instances of an element in a state
;Takes a ROW, the current ROW-NUMBER, the ELEMENT to find
(defun get-elem-pos (s row-num elem)
	(cond
		((null s) nil)
		;build the list of coordinates
		(t (append
			;add all instances of the element in this row to the list
			(get-elem-pos-rows (car s) row-num 0 elem nil) 
			(get-elem-pos (cdr s) (+ row-num 1) elem)
		   ) 
		)
	)
)

;get-elem-pos tests
; (setq gbtest '(
; 	   (1 1 1 1 1 1 1)
; 	   (1 0 2 0 2 0 1)
; 	   (1 0 0 2 4 0 1)
; 	   (1 4 5 6 5 4 1)
; 	   (1 0 0 2 0 0 1)
; 	   (1 0 0 4 2 0 1)
; 	   (1 1 1 1 1 1 1)
; 	)
; )
; (format t "getting boxes (get-elem-pos gbtest 0 2) = ~S~%" (get-elem-pos gbtest 0 2))
; (format t "getting goals (get-elem-pos gbtest 0 4) = ~S~%" (get-elem-pos gbtest 0 4))

;heuristic helper that computes the straight line distance between two elements
;takes two ELEMENT POSITIONS to compute the distance between
(defun dist (e1 e2)
	(let (
			(x1 (car e1))
			(y1 (cadr e1))
			(x2 (car e2))
			(y2 (cadr e2))
		)
		;dist = sqrt((x2-x1)^2 + (y2-y1^2))
		(sqrt 
			(+ (expt (- x2 x1) 2) (expt (- y2 y1) 2))
		)
	)
)


;heuristic helper that computes the minimum distance between a box and a list of goals
;takes the coordinates of a BOX, a list of coordinates of GOALS, and MIN comparator
(defun min-dist (box goals min)
	(cond
		((null goals) min)
		;if the top of the list is less than the comparator use it as the new comparator
		((< (dist box (car goals)) min) 
			(min-dist box (cdr goals) (dist box (car goals)))
		)
		(t (min-dist box (cdr goals) min))
	)
)

;heursitic helper that gets the maximum of minimum distances between a list of boxes and a goal
;a minimum distance is defined in the min-dist helper
;takes a list of coordinates of BOXES, takes a list of coordinates of GOALS, the MAX possible BOX-GOAL DISTANCE, 
;and a comparator for the MAX
(defun max-min-dist (boxes goals max-bg-dist max)
	(cond
		((null boxes) max)
		;If the min dist for this box is greater than the max, make it the new max
		((> (min-dist (car boxes) goals max-bg-dist) max)
			(max-min-dist (cdr boxes) goals max-bg-dist (min-dist (car boxes) goals max-bg-dist))
		)
		(t (max-min-dist (cdr boxes) goals max-bg-dist max))
	)
)

;Heuristic function that uses the maximum minimum distance of all boxes to a goal
;This heuristic is admissible because the straight line distance to a goal is always
;going to be <= the number of moves to get to that goal, and each box needs to be moved
;onto a goal.
(defun h2 (s)
	(let (
			(boxes (get-elem-pos s 0 box))
			(goals (get-elem-pos s 0 star))
			(max-bg-dist (* (length s) (length (car s))))
		)
		(floor (max-min-dist boxes goals max-bg-dist 0))
	)
)

;heuristic helper that computes the sum of minumum straight line distances beteeen boxes and a goal
;takes a list of BOXES, a list of GOALS, the MAX possible BOX-GOAL DISTANCE, 
;and an accumulator for the SUM
(defun sum-min-dists (boxes goals max-bg-dist sum)
	(cond
		((null boxes) sum)
		;add each boxes min distance to the sum accumulator
		(t (sum-min-dists 
				(cdr boxes) goals max-bg-dist 
				(+ sum (min-dist (car boxes) goals max-bg-dist))
		   )
		)
	)
)

;Heuristic function that uses the sum of minimum distances of all boxes to a goal
;This heuristic is admissible because the straight line distance to a goal is always
;going to be <= the number of moves to get to that goal, and each box needs to be moved
;onto a goal.
(defun h3 (s)
	(let (
			(boxes (get-elem-pos s 0 box))
			(goals (get-elem-pos s 0 star))
			(max-bg-dist (* (length s) (length (car s))))
		)
		(floor (sum-min-dists boxes goals max-bg-dist 0))
	)
)

;heuristic helper that computes the tile distance between two elements
;takes two ELEMENT POSITIONS to compute the distance between
(defun tile-dist (obj1 obj2)
	(let (
			(x1 (car obj1))
			(y1 (cadr obj1))
			(x2 (car obj2))
			(y2 (cadr obj2))
		)
		;tile_dist = |x2-x1| + |y2-y1|
		(+ (abs (- x1 x2)) (abs (- y1 y2)))
	)
)

;heuristic helper that computes the minimum tile distance between a box and a list of goals
;takes the coordinates of a BOX, a list of coordinates of GOALS, and MIN comparator
(defun min-tile-dist (box goals min)
	(cond
		((null goals) min)
		;if the top of the list is less than the comparator use it as the new comparator
		((< (tile-dist box (car goals)) min) 
			(min-tile-dist box (cdr goals) (tile-dist box (car goals)))
		)
		(t (min-tile-dist box (cdr goals) min))
	)
)

;heuristic helper that computes the sum of minumum tile distances beteeen boxes and a goal
;takes a list of BOXES, a list of GOALS, the MAX possible BOX-GOAL TILE DISTANCE, 
;and an accumulator for the SUM
(defun sum-min-tile-dists (boxes goals max-bg-dist sum)
	(cond
		((null boxes) sum)
		;add each boxes min-tile-distance to the sum accumulator
		(t (sum-min-tile-dists 
				(cdr boxes) goals max-bg-dist 
				(+ sum (min-tile-dist (car boxes) goals max-bg-dist))
		   )
		)
	)
)

;Heuristic function that uses the sum of minimum tile distances of all boxes to a goal
;This heuristic is admissible because the minumum tile distance to a goal is always
;going to be <= the number of moves to get to that goal, and each box needs to be moved
;onto a goal.
(defun h4 (s)
	(let (
			(boxes (get-elem-pos s 0 box))
			(goals (get-elem-pos s 0 star))
			(max-bg-dist (+ (length s) (length (car s))))
		)
		(sum-min-tile-dists boxes goals max-bg-dist 0)
	)
)

(defun h5 (s)
	(let (
			(top-box (car (get-elem-pos s 0 box)))
			(goals (get-elem-pos s 0 star))
		)
		(min-tile-dist top-box goals 0)
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

;I did some random sampliing of testing with each of the heuristics for run time. I expected the 
;sum min dist or sum tile dist to do the best however I was surprised to find that these heuristics 
;performed very slowly compared to h0 on several inputs. I deduced that doing multiple recursive fn calls
;at each round even if they are tail recursive massively slows down the a* search even if
;you are checking fewer nodes. Therefore I went with a more simply computed heuristic that still provides
;more information than h0
(defun h303652195 (s)
	h1(s)
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

;(80,7) 0.0418 sec
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 0 0 4 1)
	   (1 1 1 1 1 1)))
;h0 0.0396
;h1 0.0419
;h2 0.0366
;h3 0.295
;h4 0.029


;(110,10) 0.0679sec
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 0 0 1 0 1)
	   (1 1 1 1 1 1 1)))
;h0 0.0658
;h1 0.0608
;h2 0.0725
;h3 0.0709
;h4 0.0490

;(211,12) 0.158sec
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))
;h0 0.132
;h1 0.137
;h2 0.060
;h3 0.334
;h4 0.0619

;(300,13) 0.136sec
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 0 0)
	   (0 3 1 0 0 0 0)))
;h0 0.134
;h1 0.169
;h2 0.118
;h3 0.109
;h4 0.081

;(551,10) 0.201sec
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 0 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))
;h0 0.199
;h1 0.149
;h2 0.506
;h3 0.151
;h4 0.111

;(722,12) 0.273sec
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 0 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))
;h0 0.274
;h1 0.200
;h2 0.225
;h3 0.174
;h4 0.145

;(1738,50) 1.909sec
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 0 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))
;h0 1.79
;h1 1.85
;h2 2.20
;h3 2.16
;h4 2.00

;(1763,22) 0.748sec
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 0 0 4 1)
	   (1 1 1 1 1 1)))
;h0 0.76
;h1 1.09
;h2 1.19
;h3 0.906
;h4 0.807

;(1806,41) 1.384sec
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 0 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))
;h0 1.10
;h1 1.60
;h2 1.72
;h3 1.60
;h4 1.86


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
;h0 5.89
;h1 7.79
;h2 12.89
;h3 12.55
;h4 10.94

;(16517,48)
(setq p11 '((1 1 1 1 1 1 1)
	    (1 4 0 0 0 4 1)
	    (1 0 2 2 1 0 1)
	    (1 0 2 0 1 3 1)
	    (1 1 2 0 1 0 1)
	    (1 4 0 0 4 0 1)
	    (1 1 1 1 1 1 1)))
;h0 8.625
;h1 10.05
;h2 15.67
;h3 13.07
;h4 12.53

;(22035,38)
(setq p12 '((0 0 0 0 1 1 1 1 1 0 0 0)
	    (1 1 1 1 1 0 0 0 1 1 1 1)
	    (1 0 0 0 2 0 0 0 0 0 0 1)
	    (1 3 0 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 2 1 1 1 0 0 0 1)
	    (1 0 0 0 0 1 0 1 4 0 4 1)
	    (1 1 1 1 1 1 0 1 1 1 1 1)))
;h4 (13706,38)

;(26905,28)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 4 0 0 0 0 0 2 0 1)
	    (1 0 2 0 0 0 0 0 4 1)
	    (1 0 3 0 0 0 0 0 2 1)
	    (1 0 0 0 0 0 0 0 0 1)
	    (1 0 0 0 0 0 0 0 4 1)
	    (1 1 1 1 1 1 1 1 1 1)))
;h4 (14924,28)

;(41715,53)
(setq p14 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 0 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 0 0)
	    (0 0 1 4 0 0 0)))
;h4 (34405,53)

;(48695,44)
(setq p15 '((1 1 1 1 1 1 1)
	    (1 0 0 0 0 0 1)
	    (1 0 0 2 2 0 1)
	    (1 0 2 0 2 3 1)
	    (1 4 4 1 1 1 1)
	    (1 4 4 1 0 0 0)
	    (1 1 1 1 0 0 0)
	    ))
;h4 (37901,44)

;(91344,111)
(setq p16 '((1 1 1 1 1 0 0 0)
	    (1 0 0 0 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))
;h4 (93894,111)

;(3301278,76)
(setq p17 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 0 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))
;h4 ()

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
