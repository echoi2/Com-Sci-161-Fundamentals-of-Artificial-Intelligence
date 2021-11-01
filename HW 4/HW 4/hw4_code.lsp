;
; Graph coloring to SAT conversion
;
; All functions you need to write are marked with 'EXERCISE' in their header comments.
; Same rules apply regarding Lisp functions you are allowed to use.
; In fact, you do not need a lot of Lisp functions to finish this assignment.
;

;;;;;;;;;;;;;;;;;;;;;;
; General util.
;
(defun reload()
  (load "hw4.lsp")
  );end defun

; EXERCISE: Fill this function.
; returns the index of the variable
; that corresponds to the fact that 
; "node n gets color c" (when there are k possible colors).
;
(defun node2var (n c k)
  (+ (* (- n 1) k) c)
  )

;(print (node2var 1 2 3))


; Helper function for at-least-one-color that generates a clause.
; While initially passing in an empty list, we keep on appending the variable index at c into the empty list
; recursively until we reach the value k which is the stopping condition. This creates the clause that represents
; the constraint that node n must be colored with at least one color whose index comes from set {c, c+1, ..., k}.
(defun at-least-one-color-helper (n c k list)
  (cond ((= c k) (append list (list (node2var n c k))))
	(t (at-least-one-color-helper n (+ c 1) k (append list (list (node2var n c k)))))
	)
  )

; EXERCISE: Fill this function
; returns *a clause* for the constraint:
; "node n gets at least one color from the set {c,c+1,...,k}."
;
; We create the empty list that is passed into the helper function where it gets values appended to it.
(defun at-least-one-color (n c k)
  (let ( (a (list )) )
    (at-least-one-color-helper n c k a)
    )
  )



;(print (at-least-one-color 1 2 4))


; Grabs the current variable index value using the current c value and creates negation clauses against all other
; variable index values using the changing c_new values (c_new starts as c+1 and increases by 1) using recursion until c_new is equalt to k.
; Example:
; ! -> means not(negation)
; a, b -> variable indices for colors
; we don't want a and b to be equal, so
; !(a ^ b) = (! a v !b) = (-a -b) 
(defun 1st-ele-check (n c c_new k temp_list)
  (let ( (a (node2var n c k)) (b (node2var n c_new k)) )
    (cond ((= c_new k)
	   (append temp_list (list (list (- a)(- b)))))
	  
	  (t(1st-ele-check n c (+ c_new 1) k (append temp_list (list (list (- a) (- b)))))
	    )
	  )
    )
  )

; Helper that helps to change the c and the c_new starting point values in 1st-ele-ckeck helper function.
(defun amoc-helper (n c k list)
  (cond ((= c k)
      list)
    (t (amoc-helper n (+ c 1) k (1st-ele-check n c (+ c 1) k list))))
  )



; EXERCISE: Fill this function
; returns *a list of clauses* for the constraint:
; "node n gets at most one color from the set {c,c+1,...,k}."
;
(defun at-most-one-color (n c k)
  (amoc-helper n c k (list ))
  )

;(print (at-most-one-color 1 3 8))


; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "node n gets exactly one color from the set {1,2,...,k}."
; creates a list using both at-least-one-color and at-most-one-color functions to arrive at the clause
; that ensures that node n gets exactly one color from the set {1, 2, ...., k}.
(defun generate-node-clauses (n k)
  (cons (at-least-one-color n 1 k) (at-most-one-color n 1 k))
  )



; Helper function for generate-edge-clauses that functions almost identically to the at-most-one-color-helper function.
; This is used to generate a list of clauses that ensures that the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}.
; We generate the clauses recursively by using parameter i as a counter that goes up to k which tells the function to execute the base case.
; We append the negations to an empty list (similarly to the at-most-one-color-helper function).
(defun generate-edge-clauses-helper (e i k temp_list)
  (let ((x (car e)) (y (cadr e)))
    (cond ((= i k) (append temp_list (list (list (- (node2var x i k))(- (node2var y i k)))) ))
	  (t (generate-edge-clauses-helper e (+ i 1) k (append temp_list (list (list (- (node2var x i k)) (- (node2var y i k))))) ) )
	  )
    )
  )

; EXERCISE: Fill this function
; returns *a list of clauses* to ensure that
; "the nodes at both ends of edge e cannot have the same color from the set {1,2,...,k}."
;
(defun generate-edge-clauses (e k)
  (generate-edge-clauses-helper e 1 k (list ))
  )

;(print (generate-edge-clauses '(1 3) 9))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Your exercises end here. Below are top-level
; and utility functions that you do not need to understand.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 
; Top-level function for converting the graph coloring problem
; of the graph defined in 'fname' using k colors into a SAT problem.
; The resulting SAT problem is written to 'out-name' in a simplified DIMACS format.
; (http://www.satcompetition.org/2004/format-solvers2004.html)
;
; This function also returns the cnf written to file.
; 
; *works only for k>0*
;
(defun graph-coloring-to-sat (fname out-name k)
  (progn
    (setf in-path (make-pathname :name fname))
    (setf in (open in-path :direction :input))
    (setq info (get-number-pair-from-string (read-line in) #\ ))
    (setq cnf nil)
    (do ((node 1
	              (+ node 1)
		             ))
	((> node (car info)))
      (setq cnf (append (generate-node-clauses node k) cnf))
      );end do
    (do ((line (read-line in nil 'eof)
	              (read-line in nil 'eof)))
	((eql line 'eof) (close in))
      (let ((edge (get-number-pair-from-string line #\ )))
	(setq cnf (append (generate-edge-clauses edge k) cnf))
	);end let
      );end do
    (close in)
    (write-cnf-to-file out-name (* (car info) k) cnf)
    (return-from graph-coloring-to-sat cnf)
    );end progn  
  );end defun

;
; A utility function for parsing a pair of integers.
; 
(defun get-number-pair-from-string (string token)
  (if (and string token)
      (do* ((delim-list (if (and token (listp token)) token (list token)))
            (char-list (coerce string 'list))
            (limit (list-length char-list))
            (char-count 0 (+ 1 char-count))
            (char (car char-list) (nth char-count char-list))
            )
           ((or (member char delim-list)
                (= char-count limit))
            (return
               (if (= char-count limit)
                   (list string nil)
                   (list (parse-integer (coerce (butlast char-list (- limit char-count))
                                 'string))
                         (parse-integer (coerce (nthcdr (+ char-count 1) char-list) 'string))
			  )))))))

;
; Writes clause to file handle 'out'.
;
(defun write-clause-to-file (out clause)
  (cond ((null clause) (format out "0~%"))
	(t (progn 
	          (format out "~A " (car clause))
		       (write-clause-to-file out (cdr clause))
		            );end progn
	      );end t
	);end cond
  );end defun

;
; Writes the formula cnf with vc variables to 'fname'.
;
(defun write-cnf-to-file (fname vc cnf)
  (progn
    (setf path (make-pathname :name fname))
    (setf out (open path :direction :output))
    (setq cc (length cnf))  
    (format out "p cnf ~A ~A~%" vc cc)
    (dolist (clause cnf)
      (write-clause-to-file out clause)
      );end dolist
    (close out)
    );end progn
  );end defun

;(graph-coloring-to-sat "graph2.txt" "sat.txt" 8)
