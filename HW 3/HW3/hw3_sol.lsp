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
(defun goal-test (s)
  ;you've exhausted all the squares in your check and found all boxes are in the goal
  (cond ((null s) T)
	;if the current square is an item and that item is a box, we return nil cause a box was found
	((atom s) 
	 (if (isBox s)
	     nil
	   t
	   )
	;since none of the two conditions were met above, we split the list s and search recursively to check every single square for a box.
	;if no boxes are found then both functions using parameters first s and rest s should return true.
	(t(and (goal-test (car s)) (goal-test (cdr s))))
	)
	)
  )


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

; An apology in advance: There's a lot of helper functions that I used so I'm sorry about that
; getKeeperPosition (s row) returns as (c r)
(defun try-move (s dir)
  ; define local variables for the keeper's x and y positions to find the current location on the map
  (let* ((keeperPos (getKeeperPosition s 0))
	 (x (car keeperPos))
	 (y (cadr keeperPos)))
    ; depending on the direction input, we execute different helper functions
    ; UP and DOWN cause check-vert to execute 
    ; LEFT and RIGHT cause check-hor to execute
    ; however to execute these functions we need to check if the player movement will
    ; cause collision with a wall (out of bounds is considered a wall too which is taken care of by the get-square funct)
    ; in which case null is returned
    (cond ((eq dir 'UP)
	   ; the element -> the object right above the keeper
	   (let ((element (get-square s (- y 1) x)))
		 (if (isWall element)
		     nil
		   (check-vert s x y (- y 1) element 'UP)
		   ) ; closes if
		 ) ; closes the first '(' after let
	   ) ; closes cond
	  ((eq dir 'DOWN)
	   ; the element -> the object below the keeper
	   (let ((element (get-square s (+ y 1) x)))
		 (if (isWall element)
		     nil
		   (check-vert s x y (+ y 1) element 'DOWN)
		   )
		 )
	   )
	  ((eq dir 'LEFT) 
	   ; element -> object directly left to the keeper
	   (let ((element (get-square s y (- x 1))))
		 (if (isWall element)
		     nil
		   (check-hor s x (- x 1) y element 'LEFT)
		   )
		 )
	   )
	  ((eq dir 'RIGHT)
	   ; element -> object directly right to the keeper
	   (let ((element (get-square s y (+ x 1))))
		 (if (isWall element)
		     nil
		   (check-hor s x (+ x 1) y element 'RIGHT)
		   ) ; closes if
		 ) ; closes let
	   ) ; closes eq dir RIGHT 
	  ) ; closes cond
    ) ; closes let*
  ); closes funct



; Function for when the the current element is keeperstar
; e is the element that is in front of the keeper in the direction that the keeper wants to move
; which was found in the try-move function
; if the space is blank, set the old position from keeperstar to star and the new position from blank to keeper
; else if you collide with a box, depending on the direction of the keeper we want to check 2 positions in front of the keeper
; to see if the box moving would be an illegal move.
; else if we use the same logic as the box for if element is a box on top of a goal
; both get passed on into a helper function
; else if the element is a goal, then we change the old position from keeperstar to star and the new position from star to keeperstar
(defun keeperstar-vert-updates (s c old_r new_r e dir)
  (cond ((isBlank e) (set-square (set-square s old_r c star) new_r c keeper))
	((isBox e)
	 (cond ((eq dir 'UP) (check-box-keeperstar-vert s c old_r new_r (- new_r 1)))
	       ((eq dir 'DOWN) (check-box-keeperstar-vert s c old_r new_r (+ new_r 1)))
	       )
	 )
	((isBoxStar e)
	 (cond ((eq dir 'UP) (check-boxstar-keeperstar-vert s c old_r new_r (- new_r 1)))
	       ((eq dir 'DOWN) (check-boxstar-keeperstar-vert s c old_r new_r (+ new_r 1)))
	       )
	 )
	((isStar e)
	 (set-square (set-square s old_r c star) new_r c keeperstar))
	)
  )




; The exact same function as keeperstar-vert-updates except it's used for checking the horizontal movement of the keeper
; when the keeper is on top of the goal
(defun keeperstar-hor-updates (s old_c new_c r e dir)
  (cond ((isBlank e) (set-square (set-square s r old_c star) r new_c keeper))
	((isBox e)
	 (cond ((eq dir 'RIGHT) (check-box-keepstar-hor s old_c new_c (+ new_c 1) r))
	       ((eq dir 'LEFT) (check-box-keepstar-hor s old_c new_c (- new_c 1) r))
	       )
	 )
	((isBoxStar e)
	 (cond ((eq dir 'RIGHT) (check-boxstar-keeperstar-hor s old_c new_c (+ new_c 1) r))
	       ((eq dir 'LEFT) (check-boxstar-keepstar-hor s old_c new_c (- new_c 1) r))
	       )
	 )
	((isStar e)
	 (set-square (set-square s r old_c star) new_r c keeperstar))
	)
  )


; this function checks the vertical movement of the player
(defun check-vert (s c old_r new_r e dir)
  ; curr_element can only ever be keeper or keeperstar b/c get-square is executeed using the original coordinates of the keeper without any movement updates
  (let ((curr_element (get-square s old_r c)))
    ; therefore if curr_element == keeperstar
    ; then we execute seperate operations for keeperstar
    (if (isKeeperStar curr_element) 
	(keeperstar-vert-updates s c old_r new_r e dir)
      ; otherwise curr_element is keeper
      ; we then proceed to execute operations on the keeper
      ; the conditional statements below execute in the exact same manner as seen
      ; by the keeperstar-vert-updates and the keeperstar-hor-updates functions
      (cond ((isBlank e) 
	     (set-square (set-square s old_r c blank) new_r c keeper))
	    ((isStar e) 
	     (set-square (set-square s old_r c blank) new_r c keeperstar))
	    ((isBox e) 
	     (cond ((eq dir 'UP) (check-box-vert s c old_r new_r (- new_r 1)))
		   ((eq dir 'DOWN) (check-box-vert s c old_r new_r (+ new_r 1)))
		   )
	     )
	    ((isBoxStar e) 
	     (cond ((eq dir 'UP) (check-boxstar-vert s c old_r new_r (- new_r 1)))
		   ((eq dir 'DOWN) (check-boxstar-vert s c old_r new_r (+ new_r 1)))
		   )
	     )
	    ) ; closes outer conditional statement
      ) ; closes if
    ) ; closes let
  )

; exact same function as check-vert except it checks for the objects in the path of the keeper when
; the keeper moves horizontally
(defun check-hor (s old_c new_c r e dir)
  (let ((curr_element (get-square s r old_c)))
    (if (isKeeperStar curr_element)
	(keeperstar-hor-updates s old_c new_c r e dir)
      (cond ((isBlank e) 
	     (set-square (set-square s r old_c blank) r new_c keeper))
	    ((isStar e) 
	     (set-square (set-square s r old_c blank) r new_c keeperstar))
	    ((isBox e) 
	     (cond ((eq dir 'RIGHT) (check-box-hor s old_c new_c (+ new_c 1) r))
		   ((eq dir 'LEFT) (check-box-hor s old_c new_c (- new_c 1) r))
		   )
	     )
	    ((isBoxStar e)
	     (cond ((eq dir 'RIGHT) (check-boxstar-hor s old_c new_c (+ new_c 1) r))
		   ((eq dir 'LEFT) (check-boxstar-hor s old_c new_c (- new_c 1) r))
		   )
	     )
	    )
      )
    )
  )

(defun check-box-vert (s c old_r old1_r new2_r)
  ; the r passed into this function is the original r of the keeper + 2 if a vertical direction of movement was chosen
  ; the c passed into this function is the original c of the keeper + 2 if a horizontal direction of movement was chosen
  ; this checks to see if the element above the current box is a box or a wall
  ; if it is, then we return nil because we cannot make a move then
  ; otherwise it is a moveable space
  (let ((element (get-square s new2_r c)))
    (cond ((or (isWall element) (isBox element) (isBoxStar element)) nil)
	  ((isBlank element) 
	   (set-square (set-square (set-square s new2_r c box) old1_r c keeper) old_r c blank))
	  ((isStar element) 
	   (set-square (set-square (set-square s new2_r c boxstar) old1_r c keeper) old_r c blank))
	  )
    )
  )

; moving a box that is on the goal in the vertical direction
(defun check-boxstar-vert (s c old_r old1_r new2_r)
  ; elemenet is the current object that is above or below the box
  (let ((element (get-square s new2_r c)))
    (cond ((or (isWall element) (isBox element) (isBoxStar element)) nil)
          ((isBlank element) (set-square (set-square (set-square s new2_r c box) old1_r c keeperstar) old_r c blank))
	  ((isStar element) (set-square (set-square (set-square s new2_r c boxstar) old1_r c keeperstar) old_r c blank))
          )
    )
  )

; moving a box that is on a goal while the keeper is also on goal
(defun check-boxstar-keeperstar-vert (s c old_r old1_r new2_r)
  (let ((e (get-square s new2_r c)))
    (cond ((or (isWall e) (isBox e) (isBoxStar e)) nil)
	  ((isBlank e) (set-square (set-square (set-square s new2_r c box) old1_r c keeperstar) old_r c star))
	  ((isStar e) (set-square (set-square (set-square s new2_r c boxstar) old1_r c keeperstar) old_r c star))
	  )
    )
  )

; for when the keeper is moving off of the goal vertically with a box in front of them 
(defun check-box-keeperstar-vert (s c old_r old1_r new2_r)
  ; the r passed into this function is the original r of the keeper + 2 if a vertical direction of movement was chosen
  ; the c passed into this function is the original c of the keeper + 2 if a horizontal direction of movement was chosen
  ; this checks to see if the element above the current box is a box or a wall
  ; if it is, then we return nil because we cannot make a move then
  ; otherwise it is a moveable space
  (let ((element (get-square s new2_r c)))
    (cond ((or (isWall element) (isBox element) (isBoxStar element)) nil)
          ((isBlank element) (set-square (set-square (set-square s new2_r c box) old1_r c keeper) old_r c star))
	  ((isStar element) (set-square (set-square (set-square s new2_r c boxstar) old1_r c keeper) old_r c star))
	  )
    )
  )


; moving a box that is on the goal off of the goal in the horizontal direction
(defun check-boxstar-hor (s old_c old1_c new2_c r)
  ; the r passed into this function is the original r of the keeper + 2 if a vertical direction of movement was chosen
  ; the c passed into this function is the original c of the keeper + 2 if a horizontal direction of movement was chosen
  ; this checks to see if the element above the current box is a box or a wall
  ; if it is, then we return nil because we cannot make a move then
  ; otherwise it is a moveable space
  (let ((element (get-square s r new2_c)))
    (cond ((or (isWall element) (isBox element) (isBoxStar element)) nil)
          ((isBlank element)
	   (set-square (set-square (set-square s r new2_c box) r old1_c keeperstar) r old_c blank))
	  ((isStar element) (set-square (set-square (set-square s r new2_c boxstar) r old1_c  keeperstar) r old_c blank))
	  )
    )
  )

; for when the keeper is moving off of the goal horizontally with a box in front of them
(defun check-box-keepstar-hor (s old_c old1_c new2_c r)
  (let ((element (get-square s r new2_c)))
    (cond ((or (isWall element) (isBox element) (isBoxStar element)) nil)
          ((isBlank element) (set-square (set-square (set-square s r new2_c box) r old1_c keeper) r old_c star))
	  ((isStar element) (set-square (set-square (set-square s r new2_c boxstar) r old1_c keeper) r old_c star))
          )
    )
  )

; for when the keeper is on top of the goal and moving in the horizontal direction and the box in front of the keeper
; is on top of a goal
(defun check-boxstar-keepstar-hor (s old_c old1_c new2_c r)
  (let ((element (get-square s r new2_c)))
    (cond ((or (isWall element) (isBox element) (isBoxStar element)) nil)
          ((isBlank element) (set-square (set-square (set-square s r new2_c box) r old1_c keeperstar) r old_c star))
          ((isStar element) (set-square (set-square (set-square s r new2_c boxstar) r old1_c keeperstar) r old_c star))
          )
    )
  )



(defun check-box-hor (s old_c old1_c new2_c r)
  ; the r passed into this function is the original r of the keeper + 2 if a vertical direction of movement was chosen
  ; the c passed into this function is the original c of the keeper + 2 if a horizontal direction of movement was chosen
  ; this checks to see if the element above the current box is a box or a wall
  ; if it is, then we return nil because we cannot make a move then
  ; otherwise it is a moveable space
  (let ((element (get-square s r new2_c)))
    (cond ((or (isWall element) (isBox element) (isBoxStar element)) nil)
          ((isBlank element) 
	   (set-square (set-square (set-square s r new2_c box) r old1_c keeper) r old_c blank))
	  ((isStar element) 
	   (set-square (set-square (set-square s r new2_c boxstar) r old1_c keeper) r old_c blank))
          )
    )
  )


; s = state
; r = row
; c = column
; returns the contents of the square at (r, c)
; top left is (0,0) and moves from top left to bottom right
(defun get-square (s r c)
  ; if the row value enetered is less than 0 or
  ; if the list is exhausted then
  ; you've hit a wall
  (cond ((or (< r 0) (null s)) wall)
	; we've found the desired row
	; so we then want to parse the columns in that row until we
	; can find the desired element
	((= r 0) (column-parse-element (car s) c))
	; otherwise we recurse down the list and move to the next row until
	; we reach the end.
	(t (get-square (cdr s) (- r 1) c))
	)
  )

; helper function for get-square
(defun column-parse-element(s c)
  ; if the column value entered is negative or if you exhaust the list
  ; then that means you've hit a wall
  (cond ((or (< c 0)(null s)) wall)
	; if c = 0 then
	; that means that the column desired was found so you'd want to return
	; what the element at that index is
	((= c 0) (car s))
	; otherwise we recurse and start checking the next column over
	(t(column-parse-element (cdr s) (- c 1)))
	)
  )

; recursively parses s to find the row that we want and then calls a helper function
; which changes the element at the row column index to square content v
; it then cons that list back to the original list to give you a new state
(defun set-square (s r c v)
  (cond ((or (< r 0) (null s)) nil)
	((= r 0) (cons (change-element (car s) c v) (cdr s)))
	(t (cons (car s) (set-square (cdr s) (- r 1) c v)))
	)
  )

; helper function for set-square
(defun change-element (s c v)
  (cond ((or (< c 0) (null s)) nil)
	; we use cons here instead of append because we're adding a non-list element to a list.
	; we're not combining lists here
	((= c 0) (cons v (cdr s)) (cons v (cdr s)))
	(t (cons (car s) (change-element (cdr s) (- c 1) v)))
	)
  )

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 'UP) (try-move s 'DOWN) (try-move s 'LEFT) (try-move s 'RIGHT)))
	 
	 )
    (cleanUpList result);end
   );end let
  );

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0
  )

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
;
(defun h1 (s)
  (cond ((null s) 0 )
	((atom s)
	 (if (isBox s)
	     1
	   0)
	 (t (+ (h1 (car s)) (h1 (cdr s)))
	    )
	 )
	)
  )

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
(defun h905368197 (s)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are roughly ordered by their difficulties.
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


;(setq s3 '((1 1 1 1 1) (1 0 0 6 1) (1 0 2 0 1) (1 0 0 0 1) (1 0 0 0 1) (1 1 1 1 1)) )
;(setq s1 '((1 1 1 1 1)
; (1 0 0 4 1)
; (1 0 2 0 1)
; (1 0 3 0 1)
; (1 0 0 0 1)
; (1 1 1 1 1)
; ))
;(setq s2 '((1 1 1 1 1)
; (1 0 0 4 1)
; (1 0 2 3 1)
; (1 0 0 0 1)
; (1 0 0 0 1)
; (1 1 1 1 1)
; ))
;(setq s4 '((1 1 1 1 1)
; (1 4 2 0 1)
; (1 0 0 0 1)
; (1 0 0 0 1)
; (1 0 5 3 1)
; (1 1 1 1 1)
; ))
;(print (next-states s3))

