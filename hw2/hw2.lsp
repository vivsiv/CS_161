;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;
;BFS Takes one argument FRINGE representing the tree to search
(defun BFS (FRINGE)
	(cond
		((null FRINGE) nil)
		((atom (car FRINGE)) (append (list (car FRINGE)) (BFS (cdr FRINGE))))
		(t (BFS (append (cdr FRINGE) (car FRINGE))))
	)
)

(format t "BFS '(ROOT) = '(ROOT) ~S~%" (equal (BFS '(ROOT)) '(ROOT)))
(format t "BFS '((((L E) F) T)) = '(T F L E) ~S~%" (equal (BFS '((((L E) F) T))) '(T F L E)))
(format t "BFS '((R (I (G (H T))))) = '(R I G H T) ~S~%" (equal (BFS '((R (I (G (H T)))))) '(R I G H T)))
(format t "BFS '(((A (B)) C (D))) = '(C A D B) ~S~%" (equal (BFS '(((A (B)) C (D)))) '(C A D B)))
(format t "BFS '((T (H R E) E)) = '(T E H R E) ~S~%" (equal (BFS '((T (H R E) E))) '(T E H R E)))
(format t "BFS '((A ((C ((E) D)) B))) = '(A B C D E) ~S~%" (equal (BFS '((A ((C ((E) D)) B)))) '(A B C D E)))

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
	(cond
		((equal S '(T T T T)) 'T)
		(t NIL)
	)
)

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
	(cond
		;Moving Just Homer
		((equal A 'h) 
			(cond
				;Can't move Homer if baby and dog are on same side
				((equal (second S) (third S)) NIL)
				;Cant move Homer if baby and poison are on same side
				((equal (second S) (fourth S)) NIL)
				;If homer is is on the East move him to the West
				((equal (first S) 'T) (list (cons 'NIL (cdr S))))
				;If Homer is on the West move him to the east
				(t (list (cons 'T (cdr S))))
			)
		)
		;Moving Homer and the baby
		((equal A 'b) 
			(cond
				;Can't move Homer and the Baby if they are not on the same side
				((not (equal (first S) (second S))) NIL)
				;If they are on the east side move them to the west side
				((equal (first S) 'T) (list (cons 'NIL (cons 'NIL (cddr S)))))
				;If they are on the west side move them to the east side 
				(t (list (cons 'T (cons 'T (cddr S)))))
			)
		)
		;Moving Homer and the dog
		((equal A 'd) 
			(cond
				;Can't move Homer and the dog if they are not on the same side
				((not (equal (first S) (third S))) NIL)
				;Can't move Homer and the dog if it leaves baby and poison on the same side
				((equal (second S) (fourth S)) nil)
				;If they are on the east side move them to the west side
				((equal (first S) 'T) (list (list 'NIL (second S) 'NIL (fourth S))))
				;If they are on the west side move them to the east side 
				(t (list (list 'T (second S) 'T (fourth S))))
			)
		)
		((equal A 'p) 
			(cond
				;Cant move Homer and the poison if they are not on the same side
				((not (equal (first S) (fourth S))) NIL)
				;Cant move Homer and the poison if it leaves the dog and baby on the same side
				((equal (second S) (third S)) NIL)
				;If they are on the east side move them to the west side
				((equal (first S) 'T) (list (list 'NIL (second S) (third S) 'NIL)))
				;If they are on the west side move them to the east side 
				(t (list (list 'T (second S) (third S) 'T)))
			)
		)
	)
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
	(let ( 
			;Define the legal states for each move for readability
			(HOMER_MOVE (NEXT-STATE S 'h))
			(BABY_MOVE (NEXT-STATE S 'b))
			(DOG_MOVE (NEXT-STATE S 'd))
			(POISON_MOVE (NEXT-STATE S 'p))
		)
		; Return a list of legal successor states for all 4 possible moves
		(append HOMER_MOVE BABY_MOVE DOG_MOVE POISON_MOVE)
	)
)

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.
(defun ON-PATH (S STATES)
	(cond
		((null STATES) NIL)
		((equal S (car STATES)) 'T)
		(t (ON-PATH S (cdr STATES)))
	)
)

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.
(defun MULT-DFS (STATES PATH)
	(cond
		;no states left to DFS return nil
		((null STATES) NIL)
		;if a DFS for the top STATE reaches the goal return that
		((let ((DFS_TOP (DFS (car STATES) PATH))) DFS_TOP DFS_TOP))
		;Do a DFS on the rest of the states
		(t (MULT-DFS (cdr STATES) PATH))
	)
)

; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
	(cond
		;If this DFS reaches the finals state return the path with the final state appended
		((FINAL-STATE S) (append PATH (list S)))
		;If S is on the path return nil
		((ON-PATH S PATH) NIL)
		;Otherwise do a DFS on all successor states
		(t (MULT-DFS (SUCC-FN S) (append PATH (list S))))
	)
)
