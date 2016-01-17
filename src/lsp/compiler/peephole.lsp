;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;
;;;;;                        Peephole Optimizer
;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; Based loosely on the peephole optimizer in Peter Norvig's book.
;;;;
;;;; The optimizer receives a list of code and literals. Code is a list of
;;;; instructions. Each instruction is a symbol, representing a label, or
;;;; a list of a symbol, representing an opcode, followed by numbers or
;;;; symbols.

;**** fun info; funs argument??

(in-package "XLSCMP")

(defun peephole-optimize (cl funs)
  (loop (if (not (peephole-optimize-one cl funs)) (return cl))))

(defun peephole-optimize-one (cl funs)
  (do* ((all-code (first cl))
	(code all-code (rest code))
	(instr (first code) (first code))
	(changed nil))
       ((or changed (null code)) changed)
       (setf changed
	     (cond
	      ((consp instr)
	       (simplify-instruction instr code all-code))
	      ((not (member instr funs))
	       (drop-label-if-not-used instr code all-code))))))


;;;;
;;;; Support Functions
;;;;

(defun find-target (label code)
  (dolist (c (rest (member label code)) (error "no code after ~s" label))
    (if (consp c) (return c))))

(defun drop-label-if-not-used (label code all-code)
  (when (not (find label all-code
		   :test #'(lambda (x y) (if (consp y) (member x y)))))
	(setf (first code) (second code) (rest code) (rest (rest code)))
	t))

(defun tension-test-jump (instr code all-code)
  (let ((ct (find-target (third instr) all-code))
	(at (find-target (fourth instr) all-code))
	(changed nil))
    (when (eq '%goto (first ct))
	  (setf (third instr) (second ct))
	  (setf changed t))
    (when (eq '%goto (first at))
	  (setf (fourth instr) (second at))
	  (setf changed t))
    (when (drop-dead-code instr code all-code)
	  (setf changed t))
    changed))

;;**** use loop here; is this ever called??
(defun drop-dead-code (instr code all-code)
  (when (and (consp (rest code)) (consp (second code)))
	(setf (rest code) (rest (rest code)))))

(defun short-operand-p (x) (<= 0 x 127))


;;;;
;;;; Data-Driven Instruction-Specific Optimizations
;;;;

(let ((table (make-hash-table :test 'eq)))
  (defun add-peephole-simplifier (sym fun) (push fun (gethash sym table)))
  (defun get-peephole-simplifiers (sym) (gethash sym table)))

(defun simplify-instruction (instr code all-code)
  (let ((funs (get-peephole-simplifiers (first instr))))
    (dolist (f funs)
      (when (funcall f instr code all-code)
	    (return t)))))

(defmacro define-peephole-simplifier (sym args &body body)
  `(add-peephole-simplifier ',sym #'(lambda ,args ,@body)))


;;;;
;;;; Test Jump and Goto Tensioning
;;;;

(dolist (s '(%test-1 %test-2 %test-arith-2))
  (add-peephole-simplifier s #'tension-test-jump))

(define-peephole-simplifier %goto (instr code all-code)
  (if (eq (second instr) (second code))
      (setf (first code) (second code) (rest code) (rest (rest code)))
      (let ((gt (find-target (second instr) all-code))
	    (changed nil))
	(when (and (eq '%goto (first gt)) (not (eq instr gt)))
	      (setf (second instr) (second gt))
	      (setf changed t))
	(when (drop-dead-code instr code all-code)
	      (setf changed t))
	changed)))


;;;;
;;;; Simplifiers for Other Opcodes
;;;;

;; (%initialize 0 ...) => (%initialize-0 ...)
(define-peephole-simplifier %initialize (instr code all-code)
  (when (eql 0 (second instr))
	(setf (first code) `(%initialize-0 ,@(rest (rest instr))))
	t))

;; (%set-one-value x) => (%set-one-value-return c x)
;; (%return c)
(define-peephole-simplifier %set-one-value (instr code all-code)
  (let ((next-instr (first (rest code))))
    (when (and (consp next-instr) (eq (first next-instr) '%return))
	  (setf (first code)
		`(%set-one-value-return ,(second next-instr) ,(second instr)))
	  (setf (rest code) (rest (rest code)))
	  t)))

;; (%set-values ...) => (%set-values-return c ...)
;; (%return c)
(define-peephole-simplifier %set-values (instr code all-code)
  (let ((next-instr (first (rest code))))
    (when (and (consp next-instr) (eq (first next-instr) '%return))
	  (setf (first code)
		`(%set-values-return ,(second next-instr) ,@(rest instr)))
	  (setf (rest code) (rest (rest code)))
	  t)))

;; (%set-values-list x y) => (%set-values-list-return c x y)
;; (%return c)
(define-peephole-simplifier %set-values-list (instr code all-code)
  (let ((next-instr (first (rest code))))
    (when (and (consp next-instr) (eq (first next-instr) '%return))
	  (setf (first code)
		`(%set-values-list-return ,(second next-instr)
					  ,(second instr)))
	  (setf (rest code) (rest (rest code)))
	  t)))

;; drop (%copy x x)
(define-peephole-simplifier %copy (instr code all-code)
  (when (= (second instr) (third instr))
	(setf (first code) (second code) (rest code) (rest (rest code)))
	t))

;; (%copy x z) => (%copy y z)
;; (%copy y z)
(define-peephole-simplifier %copy (instr code all-code)
  (let ((next-instr (first (rest code))))
    (when (and (consp next-instr)
	       (eq (first next-instr) '%copy)
	       (/= (third instr) (second next-instr))
	       (= (third instr) (third next-instr)))
	  (setf (first code) (second code))
	  (setf (rest code) (rest (rest code)))
	  t)))
