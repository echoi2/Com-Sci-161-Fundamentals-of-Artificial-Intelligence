;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

;Note to self: FRINGE in this function passes as a list
;even for single element lists. Keep in mind
(defun BFS (FRINGE)
  ;BASE CASE:
  ; if the list is empty then we return nil
  (cond ((null FRINGE) nil)
	; otherwise we check to see if the first item of the list is a list
	; and if it is a list we pass the list recursively to the BFS function
	; this makes it go from the top level to the lowest level
        ((listp (car FRINGE)) (BFS (append (cdr FRINGE)(car FRINGE))))
	; if element is not a list then grab all the nodes that are on the same level
        (t (cons (car FRINGE) (BFS(cdr FRINGE)))))
  )

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
  ;checks to see if current state returns goal state
  (cond ((equal S '(T T T T)) T)
	; otherwise false
        (t ()))
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
  ; if only homer wants to cross
    (cond ((eq A 'h)
	   ; he has to make sure that the baby isn't by herself with either the poison or the dog
	   ; otherwise we return nil
           (cond ((or (eq (second S)(third S)) (eq (second S) (fourth S))) nil)
		 ; we not homer because he is the only one changing positions relative to the other
		 ; three objects.
                 (t (list (append (list (not (first S)) (second S)) (list (third S) (fourth S)))))))
	  ; if he wants to cross with the baby
          ((eq A 'b)
	   ; has to first make sure he is with the baby
	   ; no other conditional check is mecessary cause baby will never be alone with either
	   ; the dog or the poison
           (cond ((eq (first S) (second S))
		  ; only homer and baby are changing positions relative to other 2 objects
		  (list (append (list (not (first S)) (not (second S))) (list (third S) (fourth S)))))
		 (t ())))
	  ; if he wants to cross with the dog
          ((eq A 'd)
	   ; homer and dog have to be on the same side and have to make sure the poison is not left 
	   ; with baby
           (cond ((or (eq (second S) (fourth S))(not (eq (first S) (third S)))) nil)
		 ; only homer and dog move relative to position of other 2 objects
                 (t (list (append (list (not (first S)) (second S)) (list (not (third S)) (fourth S)))))))
	  ; if he wants to cross with poison
          ((eq A 'p)
	   ; make sure he's on the same side as poison and that the dog is not left alone with baby
           (cond ((or (eq (second S) (third S)) (not (eq (first S) (fourth S)))) nil)
		 ; only homer and the poison move relative to the other 2 objects
                 (t (list (append (list (not (first S)) (second S)) (list (third S) (not (fourth S))))))))
          (t ()))
)

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.

(defun SUCC-FN (S)
  (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p))
    )

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.

(defun ON-PATH (S STATES)
  ; if the STATES have been exhausted, then S has not been visited by the DFS
  (cond ((equal STATES nil) nil)
        ((equal S (car STATES)) T)
        (t (ON-PATH S (cdr STATES))))
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
  ; all the successor states have been checked and so if it is nil, then that means there are no possible moves remaining.
(cond ((eq STATES nil) nil)
      ; we want to check to see if a successor state has a path leading to the goal state
      ; if it doesnt then we want to ignore the successor state and move on to the next successor state 
        ((eq (DFS (car STATES) PATH) nil) (MULT-DFS (cdr STATES) PATH))
	; however, if a path to the goal states does exist then we want to return the full path
        (t (DFS (car STATES) PATH)))
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
  ; if the initial state is the final state then we append the current state to the PATH to get the full path
  (cond ((FINAL-STATE S) (append PATH (list S)))
	; else if the current node has already been visited, then we return nil
        ((ON-PATH S PATH) nil)
	; else we want to check to see if any of the successor paths have a path to the goal path from the 
	; initial state
        (t (MULT-DFS (SUCC-FN S) (append PATH (list S)))))
  )

; Test Functions
; (print (BFS '(ROOT)))
; (print  (BFS '((((L E) F) T))))
; (print  (BFS '((R (I (G (H T)))))))
; (print (BFS '(((A (B)) C (D)))))
; (print (BFS '((T (H R E) E))))
; (print (BFS '((A ((C ((E) D)) B)))))

; (print (NEXT-STATE '(NIL NIL T T) 'h))
; (print (NEXT-STATE '(T NIL T T) 'h))
; (print (NEXT-STATE '(T T T T) 'b))
; (print (NEXT-STATE '(T NIL T T) 'd))
; (print (NEXT-STATE '(T NIL T T) 'd))
; (print (NEXT-STATE '(T NIL T T) 'p))
; (print (NEXT-STATE '(T NIL T T) 'p))

;(print (ON-PATH '(a b c d) '((a b c e) (a b c d))))
