; Good ol towers of hanoi
;
; Usage:
;      (hanoi <n>)
;          <n> - an integer the number of discs

(defun hanoi(n)
  ( transfer 'A 'B 'C n ))

(defun print-move ( from to )
  (princ "Move Disk From ")
  (princ from)
  (princ " To ")
  (princ to)
  (princ "\n")
  nil)


(defun transfer ( from to via n )
  (cond ((equal n 1) (print-move from to ))
	(t (transfer from via to (- n 1))
	   (print-move from to)
	   (transfer via to from (- n 1)))))


