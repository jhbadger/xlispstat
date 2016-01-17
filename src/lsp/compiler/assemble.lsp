(in-package "XLSCMP")

(defparameter *opcode-translations* nil)

(defparameter *opcode-symbols*
  '(%COPY
    %GOTO
    %ARITH2
    %ARITH-PRED2
    %SET-SVREF
    %SVREF
    %SET-AREF1
    %AREF1
    %SET-ELT
    %ELT
    %SET-ONE-VALUE
    %SET-ONE-VALUE-RETURN
    %SET-VALUES
    %SET-VALUES-RETURN
    %SET-VALUES-LIST
    %SET-VALUES-LIST-RETURN
    %CAR
    %CDR
    %RPLACA
    %RPLACD
    %CONS
    %TEST-1
    %SAVE-MVCALL
    %SAVE-CALL
    %MVCALL
    %CALL
    %SAVE-MVLCALL
    %SAVE-LCALL
    %MVLCALL
    %LCALL
    %SAVE-MVVCALL
    %SAVE-VCALL
    %MVVCALL
    %VCALL
    %MAKE-CELL
    %CELL-VALUE
    %SET-CELL-VALUE
    %TEST-ARITH-2
    %SYMVAL
    %SYMFUN
    %EQ
    %EQL
    %EQUAL
    %CONSP
    %ENDP
    %SET-GET
    %GET
    %SET-NTH
    %NTH
    %SET-SYMVAL
    %TEST-2
    %MAKE-CLOSURE
    %CATCH-BLOCK
    %THROW-RETURN-FROM
    %CATCH-TAGBODY
    %THROW-GO
    %UNWIND-PROTECT
    %RETURN
    %GET-ONE-VALUE
    %GET-VALUES
    %CASE
    %ARITH1
    %SLOT-VALUE
    %SET-SLOT-VALUE
    %SUPPLIED-P
    %CATCH
    %THROW
    %SET-AREF2
    %AREF2
    %DYNAMIC-BIND
    %DYNAMIC-UNBIND
    %CXR
    %ERRSET
    %NTH-VALUE
    %MAKE-Y-CLOSURES
    %PUSH-VALUES
    %POP-VALUES
    %INITIALIZE
    %SET-CAR
    %SET-CDR
    %INITIALIZE-0
    %STOP
    %SWAP
    %LDCONST
    %NCASE
    %MAKE-CLOSURES
    %SET-CLOSURE-DATA
    %SET-CLOSURE-CODE
    %LDNOTSUPP
    %LDMVARGS
    %NOT
    %NEW-BLOCK
    %NEW-TAGBODY
    %NEW-GO
    %NEW-CATCH
    %NEW-ERRSET
    %NEW-UNWIND-PROTECT
    %GET-OPTARG
    %MAKE-KEYARGS
    %CHECK-LAST-KEYARG
    %GET-KEYARG
    %NEW-DYNAMIC-BIND
    %NEW-DYNAMIC-UNBIND
    %STRUCT-OP))

(do* ((i 0 (+ i 1))
      (syms *opcode-symbols* (rest syms)))
     ((null syms))
     (push (cons (first syms) i) *opcode-translations*))

(defun encode-opcodes (code) (sublis *opcode-translations* code))

;;**** split into pieces?
;;**** drop the environment stuff?
(defun assemble (cl)
  (let ((code (first cl))
	(lits (coerce (second cl) 'vector))
	(fl (third cl))
	(env (if (fourth cl) (coerce (fourth cl) 'vector)))
	(jt nil)
	(ji nil)
	(fi nil))
    ;; translate opcodes
    (setf code (encode-opcodes code))
    ;; setup jumptable map
    (let ((jcount 0))
      (dolist (i code)
        (when (symbolp i)
	      (push (cons i jcount) ji)
	      (incf jcount))))
    ;; replace labels by jumptable indices
    (setf code (sublis ji code))
    ;; check for bad opcodes
    (let ((badops nil))
      (mapc #'(lambda (x)
		(if (and (consp x) (symbolp (first x))) (push x badops)))
	    code)
      (if badops
	  (error "unsupported opcodes -- ~a" (remove-duplicates badops))))
    ;; compute the jump table and flatten the code
    (setf jt (make-array (length ji)))
    (let ((count 0)
	  (ncode nil))
      (dolist (i code)
        (cond
	 ((consp i)
	  (dolist (x i)
	    (cond
	     ((<= 0 x 127) ;; short operands
	      (incf count)
	      (push x ncode))
	     ((<= 128 x 32767) ;; long operands
	      (let ((hi (floor (/ x 256)))
		    (lo (rem x 256)))
		(incf count 2)
		(push (+ hi 128) ncode)
		(push lo ncode)))
	     (t (error "operand out of range")))))
	 (t (setf (aref jt i) count))))
      (setf code (nreverse ncode)))
    ;; make the byte code
    (make-byte-code (coerce code 'vector) jt lits (cdr (assoc fl ji)) env)))
