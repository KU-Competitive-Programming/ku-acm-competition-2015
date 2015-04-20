
;node structure
(defstruct node action state f g h parent)

(defvar *open*)
(defvar *closed*)
(defvar explored)

;priority queue creation and manipulation
(defun make-empty-queue () nil)

(defun empty-queue? (queue) (null queue))

(defun enqueue (item queue)
	(cond 	((eq queue 'closed) (setq *closed* (cons item *closed*)))
	       	((eq queue 'open) (cond	((or (null *open*) (< (node-f item) (node-f (car *open*)))) (setq *open* (cons item *open*)))
					(t (do 	((x *open*)
		     				 (y (cdr *open*)))
					       	((or (null y) (< (node-f item) (node-f (car y)))) (cond	((null y) (setf (cdr x) (list item)))
						  (t (setf (cdr x) (cons item y)))))
					    (setq x y)
					    (setq y (cdr y))))))))

(defun dequeue (queue) 
	(cond	((eq queue 'open) (let ((firstnode (car *open*))) (setq *open* (cdr *open*)) firstnode))
		(t (error "Error - can only dequeue from *open*"))))

(defun change-priority (item queue)
	(cond	((eq queue 'open) (setq *open* (delete item *open*)) (enqueue item 'open))
		(t (error "Error - can only change prioritiy on *open*"))))




(defvar goal-state '((1 2 3)(4 5 6)(7 8 0)))

(defun member-of (x l)
	(equal x (find x l)))

;start at x = 0
;find (x , y) of the number n in state s
(defun object-position (n s x)
	(if (member-of n (car s)) (list x (position n (car s))) (object-position n (cdr s) (+ x 1))))

(defun number-at (x y s)
	(nth y (nth x s)))

(defun swap (s a b c d)
	(let((state (copy-tree s)))
	    (setf (nth b (nth a state)) "I")
	    (setf (nth d (nth c state)) "L")
	    state))

;check if goal is reached
(defun goal? (s)
	(equal goal-state s))

(defun distance (x s g)
	(let ((a (object-position x s 0))
	      (b (object-position x g 0)))
	     (+ (abs (- (car a) (car b))) (abs (- (cadr a) (cadr b))))))

(defun manhattan-distance-heuristic (s) 
	(distance "L" s goal-state))

(defun max-move (s a b c d)
	(if(or (>= (+ a c) (length s))  (>= (+ b d) (length (nth a s))) (< (+ a c) 0)  (< (+ b d) 0) (equal (nth (+ b d) (nth (+ a c) s)) "X")) (cons a b)
		(max-move s (+ a c) (+ b d) c d)))

(defun possible-moves (state) 
	(setq pos (object-position "L" state 0))
	(let 	((x (car pos))
		 (y (cadr pos))
		 (moves '()))
		(let ((a (max-move state x y 1 0))
			  (b (max-move state x y -1 0))
			  (c (max-move state x y 0 1))
			  (d (max-move state x y 0 -1)))
		(if (equal x (car a)) nil
			(setf moves (append moves (list (list (swap state x y (car a) y) (cons (number-at (car a) y state) "D") 1)))))
		(if (equal x (car b)) nil 
			(setf moves (append moves (list (list (swap state x y (car b) y) (cons (number-at (car b) y state) "U") 1)))))
		(if (equal y (cadr c)) nil 
			(setf moves (append moves (list (list (swap state x y x (cadr c)) (cons (number-at x (cadr c) state) "R") 1)))))
		(if (equal y (cadr d)) nil 
			(setf moves (append moves (list (list (swap state x y x (cadr d)) (cons (number-at x (cadr d) state) "L") 1))))))
	       	moves))

(defun add-state (saw current heuristic) 
	(let 	((state (car saw))
       		 (g-new (caddr saw))
	       	 (g-old (node-g current))
       		 (h (funcall heuristic (car saw))))
	       	(let ((new-node (make-node 	:action (cons (caadr saw) g-new)
						:state state
		      				:g (+ g-new g-old)
						:h h
		      				:f (+ (+ g-new g-old) h)
	   			       		:parent current)))
				(enqueue new-node 'open))))

(defun A* (start goal move heuristic)
	(setq *open* (make-empty-queue))
	(setq explored (make-hash-table :test 'equal))
	(let ((start-node (make-node 	:action "start" 
				       	:state start 
					:g 0 
					:h (funcall heuristic start) 
					:f (funcall heuristic start)
					:parent nil)))
	  (enqueue start-node 'open))
	(loop while (not (empty-queue? *open*)) do 
	     (let((s (dequeue 'open))) 
		 (if (funcall goal (node-state s)) (return-from A* s) 
		     (let((moves (funcall move (node-state s))))
			 (setf (gethash (node-state s) explored) (node-state s))
			 (setf moves (remove nil (map 'list (lambda (x) (if (gethash (car x) explored) nil  x)) moves)))
			 (map '() (lambda (x) (add-state x s heuristic)) moves)))))
	(format t "No solution, ~d nodes checked" (hash-table-count explored)))

(defun path (n) 
  (if (null n) '()
	(cons (list (node-state n)) (path (node-parent n)))))

(defun find-goal (s)
	(let ((state (copy-tree s))
		  (pos1 (object-position "L" s 0))
		  (pos2 (object-position "G" s 0)))
		 (let ((x (car pos1))
		 	   (y (cadr pos1))
		 	   (a (car pos2))
		 	   (b (cadr pos2)))
		      (setf (nth y (nth x state)) "I")
		      (setf (nth b (nth a state)) "G"))
		 state))

(defun ice-puzzle (start)
	(setq goal-state (find-goal start))
	(length (path (A* start 'goal? 'possible-moves 'manhattan-distance-heuristic))))

(defun convert-input (s)
	(map 'list (lambda (y) (map 'list (lambda (u) (coerce u 'string)) (map 'list (lambda (z) (list z)) (coerce y 'list)))) (map 'list (lambda (x) (substitute #\I #\Space x)) s)))

(defvar input '("L   " " X X" "    " "  XG"))

(setf start-state (convert-input (input)))

(ice-puzzle start-state)
