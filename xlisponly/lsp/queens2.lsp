; queens2.lsp
;
; Place n queens on a board (graphical version)
;  See Winston and Horn Ch. 11
; 
; Usage:
;	(queens <n>)
;          where <n> is an integer -- the size of the board - try (queens 4)

; Do two queens threaten each other ?
(defun threat (i j a b)
  (or (eql i a)			;Same row
      (eql j b)			;Same column
      (eql (- i j) (- a b))	;One diag.
      (eql (+ i j) (+ a b))))	;the other diagonal

; Is position (n,m) on the board safe for a queen ?
(defun conflict (n m board)
  (cond ((null board) nil)
	((threat n m (caar board) (cadar board)) t)
	(t (conflict n m (cdr board)))))


; Place queens on a board of size SIZE
(defun queens (size)
  (prog (n m board soln)
	(setq soln 0)			;Solution #
	(setq board nil)
	(setq n 1)			;Try the first row
	loop-n
	(setq m 1)			;Column 1
	loop-m
	(cond ((conflict n m board) (go un-do-m))) ;Check for conflict
	(setq board (cons (list n m) board))       ; Add queen to board
	(cond ((> (setq n (1+ n)) size)            ; Placed N queens ?
	       (print-board (reverse board) (setq soln (1+ soln))))) ; Print it
	(go loop-n)			           ; Next row which column?
	un-do-n
	(cond ((null board) (return 'Done)) 	   ; Tried all possibilities
	      (t (setq m (cadar board))		   ; No, Undo last queen placed
		 (setq n (caar board))
		 (setq board (cdr board))))

	un-do-m
	(cond ((> (setq m (1+ m)) size)          ; Go try next column
	       (go un-do-n))
	      (t (go loop-m)))))


;Print a board
(defun print-board  (board soln &aux size)
  (setq size (length board))		;we can find our own size
  (format t "\t\tSolution ~s\n\n\t" soln)
  (print-header size 1)
  (print-board-aux board size)
  (terpri))

; Put Column #'s on top
(defun print-header (size n)
  (dotimes (i size) (format t "~s " (1+ i)))
  (terpri))

(defun print-board-aux (board size &aux (row 0))
  (mapc #'(lambda (x) 
		  (format t "~s\t" (setq row (1+ row)))
		  (print-board-row (cadr x) size))
	board))
		  
(defun print-board-row (column size)
       (dotimes (i size) (princ (if (eql column (1+ i)) "Q " ". ")))
       (terpri))

