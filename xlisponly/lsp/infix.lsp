;;;
;;; An infix to prefix converter for algebraic expressions.
;;; From Winston and Horn, Second Edition, pp 185-189.
;;;
;
;	Adapted as a lisp macro by:
;		Jonathan Roger Greenblatt (jonnyg@rover.umd.edu)
;		University of Maryland at College Park
;
;
;	(usage:
;
;		[ <expr> <oper> <expr> ( <oper> <expr> ) ... ]
;
;	<expr>: a lisp expresion.
;	<oper>: =,+,-,*,/,mod.**,^
;
;	Note: [ and ] are part of the syntax, ( and ) mean this part is
;				optional.
;
;	Examples:
;
;		[a = 7 * 5 + 4]
;		[b = 7 + (sin (float a)) + (float [a / 7]) * [3 + a]]
;
;	These are expanded to:
;
;		(SETQ A (+ (* 7 5) 4))
;		(SETQ B (+ (+ 7 (SIN (FLOAT A))) (* (FLOAT (/ A 7)) (+ 3 A))))
;
;

(defun inf-to-pre (ae)
  (labels
	((weight (operator)
	  (case operator
	    (= 0)
	    (+ 1)
	    (- 1)
	    (* 2)
	    (/ 2)
	    (mod 2)
	    (** 3)
	    (^ 3)
	    (t 4)))

	(opcode (operator)
	  (case operator
	    (= 'setq)
	    (+ '+)
	    (- '-)
	    (* '*)
	    (/ '/)
	    (mod 'mod)
	    (** 'expt)
	    (^ 'expt)
	    (t (error "~s is an invalid operator" operator))))

	(inf-aux (ae operators operands)
	  (inf-iter (cdr ae)
	    operators
	    (cons (car ae) operands)))

	(inf-iter (ae operators operands)
	  (cond ((and (null ae) (null operators))
		 (car operands))
		((and (not (null ae))
		      (or (null operators)
			  (> (weight (car ae))
			     (weight (car operators)))))
		 (inf-aux (cdr ae)
			  (cons (car ae) operators)
			  operands))
		(t (inf-iter ae
			     (cdr operators)
			     (cons (list (opcode (car operators))
					 (cadr operands)
					 (car operands))
				   (cddr operands)))))))

  (if (atom ae)
      ae
      (inf-aux ae nil nil))))

(setf (aref *readtable* (char-int #\[))
  (cons :tmacro
	(lambda (f c &aux ex)
		(setf ex nil)
		(do () ((eq (peek-char t f) #\]))
			(setf ex (append ex (cons (read f) nil))))
		(read-char f)
		(cons (inf-to-pre ex) nil))))

(setf (aref *readtable* (char-int #\]))
  (cons :tmacro
	(lambda (f c)
		(error "misplaced right bracket"))))


