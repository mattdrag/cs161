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

;
; Function: scan-row-for-box
; Auxilary function of goal-test. Parameter (r) represents a row in the solution s from goal-test.
; Output: if no boxes are on a non-goal square -> T
;		  else -> nil

(defun scan-row-for-box (r)
	(cond
		((null r) T)
		((= 2 (car r)) nil) ; 2 is the integer code for a box
		(T (scan-row-for-box (cdr r)))
	);end cond
);end defun

;
; Function: goal-test
; Parameter (s) is a list of lists; therefore we want to break it down list by list using
; scan-row-for-box and check to see if there is a box on any of the rows. 
; Output: Returns true (t) if s is a proper solution.
;

(defun goal-test (s)
  	(cond
		((null s) T)
		(T (AND (scan-row-for-box (car s)) (goal-test (cdr s)) ))
	);end cond
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; NEXT-STATE CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; A list of available helper functions: isBlank, isBox, isKeeper, isStar, isBoxStar, isKeeperStar
;

;
; Function: object-at-row, set-object-at-row
; Description: Auxilary function for object-at-position, set-object-at-position.
; Input: s - row of objects
; 		 pos - postion to check
;		 val - value to be set
; Output: returns/sets whats object (1 .. 6) at postion pos
;

(defun object-at-row (s pos)
  (cond
    ((> pos (length s)) 1)							;out of bounds so just return a wall
    ((= pos 0) (car s)) 							;we are at the desired pos; return object 
    (T (object-at-row (cdr s) (- pos 1)))			;shift over one pos
  );cond
);defun

(defun set-object-at-row (s pos val)
  (cond
	((= pos 0) (cons val (cdr s)))	;arrived at pos; make row with new val
	(T (cons (car s) (set-object-at-row (cdr s) (- pos 1) val))) ;shift over one pos
  );cond
);defun

;
; Function: object-at-position, set-object-at-position
; Description: Auxilary function for try-move. Recurses until it arrives at the row we are interested in
; Input: s - current state of the board
; 		 row - row postion to check
;		 col - col postion to check
;		 val - value to be set
; Output: returns/sets whats object (1 .. 6) at postion (row,col)
;

(defun object-at-position (s row col)
  (cond
    ((> row (length s)) 1)							;out of bounds so just return a wall
    ((= row 0) (object-at-row (car s) col)) 		;we are at the desired row; get object at col
    (T (object-at-position (cdr s) (- row 1) col)) 	;shift up one row
  );cond
);defun

(defun set-object-at-position (s row col val)
  (cond
  	((null s) nil)														  ;error check
    ((= row 0) (cons (set-object-at-row (car s) col val) (cdr s))) 		  ;we are at the desired row; set object at col
    (T (cons (car s) (set-object-at-position (cdr s) (- row 1) col val))) ;shift up one row
  );cond
);defun

;
; Move functions for affecting board state

;
; Function: blankOrStar
; Description: will return a blank or a star depending on if keeper is moving off of a star or not
;

(defun blankOrStar (s row col)
  (let ( (obj (object-at-position s row col)) )
         (cond
            ((isKeeperStar obj) 4)  ; return a star
            (T 0) 					; return a blank
         );end cond        		
   );end let
);end defun

;
; Function: keeperOrKS
; Description: Auxilary for box-move. returns keeper or keeperStar if you moved box from star
;

(defun keeperOrKS (s row col)
	(let ( (obj (object-at-position s row col)) )
         (cond
            ((isBoxStar obj) 6)  ; return a keeperstar
            (T 3) 				 ; return a keeper
         );end cond        		
    );end let
);end defun

;
; Function: box-move
; Description: moves a box in direction dir
;

(defun box-move (s y x dir)
  (let* ((dx (cond 							; Assign dx to the difference in x coord if dir = RIGHT or LEFT
                     ((= dir 4) (+ x 1)) 	; Move RIGHT; x += 1
                     ((= dir 3) (- x 1))	; Move LEFT; x -= 1
                     (T x)
                );end cond
		   );end dx
           (dy (cond 						; Assign dY to the difference in y coord if dir = DOWN or UP
                     ((= dir 2) (+ y 1))	; Move DOWN; y += 1
                     ((= dir 1) (- y 1))	; Move UP; y -= 1
                     (T y)
               );end cond
           );end dy 
           (obj (object-at-position s dy dx))
          );end assignment
  		(cond 																				 ; For all conditions:   (x,y) -> keeperOrKS;
              ( (isBlank obj) (set-object-at-position (set-object-at-position s y x (keeperOrKS s y x)) dy dx 2) )  ;(dx,dy) -> box
              ( (isStar obj) (set-object-at-position (set-object-at-position s y x (keeperOrKS s y x)) dy dx 5) ) 	;(dx,dy) -> boxstar   
              (T nil) 
        );end cond
	);end let
);end defun

;
; Function: try-move
; Description: 	Auxilary function for next-states attempting to move keeper. Depending on what object is at new position, we want to
; 				change the board state.
; State Transitions: If obj isBlank (0), just move keeper.
;					 If obj isBox (2), try to move keeper and box
;					 If obj isStar (4), move and make keeper+star
;					 If obj isBoxStar (5), we dont wanna move the box
;					 Else no move possible
;
; Input: s - current state of the board
; 		 dir - direction in which you want to try and move
;			   1 = UP; 2 = DOWN; 3 = LEFT; 4 = RIGHT; 
;		 x - row position of keeper
;		 y - col position of keeper
; Output: if its a valid move, will return the game state after that move has been made
;		  else returns nil
;

(defun try-move (s dir x y)
	(let* ((dx (cond 						; Assign dx to the difference in x coord if dir = RIGHT or LEFT
                     ((= dir 4) (+ x 1)) 	; Move RIGHT; x += 1
                     ((= dir 3) (- x 1))	; Move LEFT; x -= 1
                     (T x)
                );end cond
		   );end dx
           (dy (cond 						; Assign dY to the difference in y coord if dir = DOWN or UP
                     ((= dir 2) (+ y 1))	; Move DOWN; y += 1
                     ((= dir 1) (- y 1))	; Move UP; y -= 1
                     (T y)
               );end cond
           );end dy 
           (obj (object-at-position s dy dx))
          );end assignment

		; we now want to set up the new board state depending on what object is at (dx,dy)
		(cond ;NOTE: we want to change at least 2 postions always, x,y, and dx,dY 							; For all conditions:   (x,y) -> blankOrStar;
			((= obj 0) (set-object-at-position (set-object-at-position s y x (blankOrStar s y x)) dy dx 3))	; obj at is blank; 		(dx,dy) -> keeper
            ((= obj 2) (set-object-at-position (box-move s dy dx dir) y x (blankOrStar s y x)))				; obj is a box; 		(dx,dy) -> box-move
            ((= obj 4) (set-object-at-position (set-object-at-position s y x (blankOrStar s y x)) dy dx 6)) ; obj is a star; 		(dx,dy) -> keeperStar
            ((= obj 5) (set-object-at-position (box-move s dy dx dir) y x (blankOrStar s y x)))				; obj is a boxStar;		(dx,dy) -> box-move
            (T nil)
		);end cond
	);end let
);end defun

;
; Function: next-states
; Description: Try's each valid move using try-move, and returns a list of the valid moves.
;			   cleanUpList is called at the end to remove any nil returned by try-move.
; Input: s - current state of the board 
; Output: a list of valid next board states 
;

(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	 (x (car pos))
	 (y (cadr pos))
	 ;x and y are now the coordinate of the keeper in s.
	 (result (list (try-move s 1 x y) (try-move s 2 x y) (try-move s 3 x y) (try-move s 4 x y))) ; 1 = UP; 2 = DOWN; 3 = LEFT; 4 = RIGHT;
	 );end assignment
    (cleanUpList result);end
  );end let
);end defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;; END OF NEXT-STATE CODE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s)
  0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; Q: Is this heuristic admissible?
; A: Yes. An admissible heuristic by definition will never overestimate. This function 
; 	 will calculate the number of boxes that are not currently on a goal, and since 
;	 the game is not complete until ALL boxes are on goals, h1 will never over-estimate.
;

; Function: isBoxWrapper
; Description: instead of T/F for isBox, returns a 1 or a zero
(defun isBoxWrapper (s)
    (cond
        ((isBox s) 1)
        (T 0)
    );end cond
);end defun

;
; Function: count-misplaced-boxes
; Description: Auxilary function for h1. Counts up mispaced boxes on a single row s
; Input: s - row we want to count on
; Output: number of misplaced boxes 
;

(defun count-boxes (s)
    (cond
         ((null s) 0)
         (T (+ (isBoxWrapper (car s)) (count-boxes (cdr s))) ) 
    );end cond
);end defun

(defun h1 (s)
    (cond
        ((null s) 0)	;base case
        (T (+ (h1 (cdr s)) (count-boxes (car s)) )) ;add the number of misplaced boxes on the row to the number on the rest of board
    );end cond
);end defun

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOTE: unfinished. instead of doing full implementation, just compares first box with first goal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
; Function: scan-row
; Description: Auxilary function for scan-board. scans a row s for obj

(defun scan-row  (s obj row col)
	(cond 
		((null s) nil)
		((= obj (car s)) (append (list (list col row)) (scan-row (cdr s) obj row (+ col 1))))
		(T (scan-row (cdr s) obj row (+ col 1) ) )
	);cond
);defun

;
; Function: scan-board
; Description: Auxilary function for h1. Counts up mispaced boxes on a single row s
; Input: s - board state
;		 row - row we are on. starts at 0
;		 obj - type of object we are looking for
; Output: list of coordinates of objects. 
;
(defun scan-board (s row obj)
	(cond
		((null s) nil)
		(T (append (scan-row (car s) obj row 0) (scan-board (cdr s) obj (+ row 1) ) ) )
	);cond
);defun

;
; Function: calc-grid-distance
; Description: calculate grid distance between box and star
; Output: an integer distance
(defun calc-grid-distance (bx by sx sy)
	(let* ( (dx (cond 
                ((> bx sx) (- bx sx))
                (T (- sy sx)) )
			);end dx
            (dy (cond 
                ((> by sy) (- by sy))
                (T (- sy by)) )
            );end dy
           );end assignment
          (+ dx dy) 
    	);assign
	);defun

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; NOTE: unfinished. instead of doing full implementation, just compares first box with first goal
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;
; Function: h804472271
; Description: We will base our heuristic in the following things:
;			   - : cumulative distance of remaining boxes to their closest goals. We first seek to make a pairing
;					between boxes and goals. then we add up the distances
;
(defun h804472271 (s)
  (let* ((boxList (scan-board s 0 2)) ;were always going to start at 0; 2 = box
  		 (starList (scan-board s 0 4)) ;were always going to start at 0; 4 = star
  		 (dist (calc-grid-distance (first (car boxList)) (second (car boxList)) (first (car starList)) (second (car starList)))) ;ran outta time; calc grid distance between first box and first star
  	   );assignment
  	   (+ dist 0)       		
   );end let
);end defun

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
