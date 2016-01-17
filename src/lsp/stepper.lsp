;;
;; File: STEPPER.LSP
;; Author: Ray Comas (comas@math.lsa.umich.edu)
;;
;; Modifications and corrections by Tom Almy
;; The program did not correctly handle RETURN (as reported by Martin
;; Glanvill, mcg@waikato.ac.nz). In the process of fixing the the
;; problem it was discovered that the nexting printout did not work
;; properly for all return, return-from, throw, and many cases of go.
;; This version has been fixed for hopefully all of the above, although
;; go will still not produce proper printout if the jump is outside the
;; most enclosing tagbody, and the tag arguments of catch/throw must
;; either be symbols or quoted symbols.  I'm making no attempt here to
;; correctly handle tracing of unwind-protect, either!
;; Modifications marked "TAA"
;; Tom Almy  5/92
;;-----------------------------------------
;; Modifications -
;;
;; Function : Eval-hook-function
;;
;; Modifcation :- MCG 5/5/93
;;
;;  "is-brk-in-form" function added to look in advance
;;  to see if any break points are in the current form.
;;  If not, then the stepper will step over the form
;;  without evaluating the sub-forms within the current form
;; (as original did); if break point found then it steps into
;; the form.
;; The Advantage is when you have a break point at the end of 
;; a prog with massive amounts of DO loops, you don't want to waste
;; time stepping into the do loop!
;; Also I've modified it for use on COMMON LISP and XLISP
;; See notes at bottom.
;; Problems: in CL, step into LOOP's/ PROGN's  before 
;;           excuting the "g" command!
;; Future Updates : further investigation of LOOPS and PROGn's as above.

;; Modification: TAA 5/5/93 

;; I made the Common Lisp vs. Xlisp choice automatic via conditional
;; compilation (Gee, I was hoping to find a good use for this feature!)

;; Modifications for use with XLISP-STAT:
;; moved step to XLISP package
;; removed conditionals
;; LT 2/9/94

(in-package "XLISP")

(export '(step *stepper-depth* *stepper-length*))

(defpackage "TOOLS" (:use "XLISP"))

(in-package "TOOLS")


(defmacro while (test &rest forms) `(do () ((not ,test)) ,@forms))

(defparameter *hooklevel* 0)            ;create the nesting level counter.
(defvar *stepper-depth*   3)            ;create depth counter
(defvar *stepper-length*  3)            ;create length counter
(defparameter *fcn*     '*all*)         ;create "one-shot" breakpoint specifier
(defvar *steplist*      nil)            ;create breakpoint list
(defparameter *steptrace* '(t . t))     ;create stepping flags
(defparameter *callist* nil)            ;create call list for backtrace
 

; this macro invokes the stepper - MCG 5/5/93 step -> usr-step , CL mod.
(defmacro xlisp::step (form &aux (val (gensym)))
  `(let ((,val nil))
     (setq *hooklevel*  0               ;init nesting counter
           *fcn*        '*all*          ;init break-point specifier
           *steptrace*  '(t . t))
     (setq *callist* (list (car ',form))) ;init call list
     (terpri *debug-io*)
     (step-flush)
     (princ *hooklevel* *debug-io*)
     (princ " >==> " *debug-io*)
     (prin1 ',form *debug-io*)          ;print the form
     (setq ,val (evalhook ',form        ;eval, and kick off stepper
                          #'eval-hook-function
                          nil
                          nil))
     (terpri *debug-io*)
     (princ *hooklevel* *debug-io*)     ;print returned value
     (princ " <==< " *debug-io*)
     (prin1 ,val *debug-io*)
     (terpri *debug-io*)
     ,val))                             ;and return it
 
(defun eval-hook-function (form env &aux val cmd)
  (setq *hooklevel* (1+ *hooklevel*))   ;incr. the nesting level
  (cond ((consp form)                   ;if interpreted function ...
         (step-add-level form env)  ;; add to *call-list*  TAA
         (tagbody
          (loop                         ;repeat forever ...
                                        ;check for a breakpoint
           (when (and (not (equal *fcn* '*all*))
                      (not (equal *fcn* (car form)))
                      (not (and (numberp *fcn*) (>= *fcn* *hooklevel*))))
                 (unless (and *fcn* (member (car form) *steplist*))
 
                                        ;no breakpoint reached -- continue
                         (setf (cdr *steptrace*) nil)
                         (when (car *steptrace*)
                               (setf (cdr *steptrace*) t)
                               (step-print-compressed form))

                         (cond                                ;- MCG 5/5/93
                          ((is-brk-in-form form *steplist*)   
                                  (setq val (list     form
                                                      #'eval-hook-function
                                                      nil
                                                      env)))
                          (t  (setq val (list form nil nil env))))                           
 
                                                     
                         (go next)))
 
                                        ;breakpoint reached -- fix things & get a command
           (step-print-compressed form)
           (setf (cdr *steptrace*) t)
           (setq *fcn* '*all*)          ;reset breakpoint specifier
           (princ " :" *debug-io*)      ;prompt user

           (setq cmd                    ;get command from user 
                 (get-key))
                                        ;process user's command
           (cond
            ((or (eql cmd #\n) (eql cmd #\Space)) ;step into function
             (setq val (list     form
                                 #'eval-hook-function
                                 nil
                                 env))
             (go next))
            ((or (eql cmd #\s)          ;step over function
		 (eql cmd #\Newline)

             ) ;; Added check for control-M TAA
             (setq val (list form nil nil env))
             (go next))
            ((eql cmd #\g)              ;go until breakpt. reached
             (setq *fcn* t)
             (setq val (list     form
                                 #'eval-hook-function
                                 nil
                                 env))
             (go next))
            ((eql cmd #\w)              ;backtrace
             (step-baktrace))
            ((eql cmd #\h)              ;display help
             (step-help))
            ((eql cmd #\p)              ;pretty-print form
             (terpri *debug-io*)
             (pprint form *debug-io*))
            ((eql cmd #\f)              ;set function breakpoint
             (princ "Go to fn.: " *debug-io*)
             (setq *fcn* (read *debug-io*))
             (step-flush))
            ((eql cmd #\u)              ;go up one level
             (setq *fcn* (1- *hooklevel*)))
            ((eql cmd #\b)              ;set breakpoint
             (princ "Bkpt.: " *debug-io*)
             (step-set-breaks (read *debug-io*))
             (step-flush))
            ((eql cmd #\c)              ;clear a breakpoint
             (princ "Clear: " *debug-io*)
             (step-clear-breaks (read *debug-io*))
             (step-flush))
            ((eql cmd #\t)              ;toggle trace mode
             (setf (car *steptrace*)
                   (not (car *steptrace*)))
             (princ "Trace = " *debug-io*)
             (prin1 (car *steptrace*) *debug-io*))
            ((eql cmd #\q)              ;quit stepper
             (setq *fcn* nil))
            ((eql cmd #\x)              ;evaluate a form
             (princ "Eval: " *debug-io*)
             (step-do-form (read *debug-io*) env)
             (step-flush))
            ((eql cmd #\r)              ;return given expression
             (princ "Return: " *debug-io*)
             (setq val (list (read *debug-io*) nil nil env))
             (step-flush)
             (go next))
            ((eql cmd #\#)              ;set new compress level
             (princ "Depth: " *debug-io*)
             (step-set-depth (read *debug-io*))
             (step-flush))
            ((eql cmd #\.)
             (princ "Len.: " *debug-io*)
             (step-set-length (read *debug-io*))
             (step-flush))
            ((eql cmd #\e)              ;print environment
             (step-print-env env))
            (t (princ "Bad command.  Type h for help\n" *debug-io*))))
 
          next                          ;exit from loop
          ;; call of evalhook was done prior to "go next" in the loop above.
          ;; now it's done outside the loop to solve problems handling
          ;; return.  TAA
          (step-fix-levels)
          (setq val (apply #'evalhook val))
          (step-fix-throw)
          (when (cdr *steptrace*)
                (terpri *debug-io*)
                (step-spaces *hooklevel*)
                (princ *hooklevel* *debug-io*)
                (princ " <==< " *debug-io*) ;print the result
                (prin1 val *debug-io*))
          (step-prune-level))) ;; step-prune-level replaces inline code TAA
 
                        ;not an interpreted function -- just trace thru.
        (t (unless (not (symbolp form))
                   (when (car *steptrace*)
                         (terpri *debug-io*)
                         (step-spaces *hooklevel*) ;if form is a symbol ...
                         (princ "         " *debug-io*)
                         (prin1 form *debug-io*) ;... print the form ...
                         (princ " = " *debug-io*)))
           (setq val (evalhook form nil nil env)) ;eval it
           (setq *hooklevel* (1- *hooklevel*))  ;decrement level
           (unless (not (symbolp form))
                   (when (car *steptrace*)
                         (prin1 val *debug-io*))))) ;... and the value
  val)                                  ;and return the value
 

;; Made compress local function
;; and changed name fcprt to step-print-compressed  TAA

;compress and print a form
(defun step-print-compressed (form)
       (terpri *debug-io*)
       (step-spaces (min 20 *hooklevel*))
       (princ *hooklevel* *debug-io*)
       (princ " >==> " *debug-io*)
       (let ((*print-level* *stepper-depth*)
	     (*print-length* *stepper-length*))
	    (prin1 form *debug-io*))
       (princ " " *debug-io*))
 
;a non-recursive fn to print spaces (not as elegant, easier on the gc)
(defun step-spaces (n) (dotimes (i n) (princ " " *debug-io*)))
 
;and one to clear the input buffer
(defun step-flush () (while (not (eql (read-char *debug-io*) #\newline))))
 
;print help
(defun step-help ()
  (terpri *debug-io*)
  (format *debug-io* "Stepper Commands~%" )

  (format  *debug-io* "----------------~%" )

  (format  *debug-io* " n or space - next form~%" )

  (format  *debug-io* " s or <cr>  - step over form~%" )

  (format  *debug-io* " f FUNCTION - go until FUNCTION is called~%" )

  (format  *debug-io* " b FUNCTION - set breakpoint at FUNCTION~%" )

  (format  *debug-io* " b <list>   - set breakpoint at each function in list~%" )

  (format  *debug-io* " c FUNCTION - clear breakpoint at FUNCTION~%" )
  (format  *debug-io* " c <list>   - clear breakpoint at each function in list~%" )
  (format  *debug-io* " c *all*    - clear all breakpoints~%" )
  (format  *debug-io* "          g - go until a breakpoint is reached~%" )
  (format  *debug-io* "          u - go up; continue until enclosing form is done~%" )

  
  (format   *debug-io*"          w - where am I? -- backtrace~%" )
  (format   *debug-io*"          t - toggle trace on/off~%" )
  (format  *debug-io* "          q - quit stepper, continue execution~%" )


  (format  *debug-io* "          p - pretty-print current form (uncompressed)~%" )
  (format  *debug-io* "          e - print environment~%" )
  (format  *debug-io* "   x <expr> - execute expression in current environment~%" )
  (format  *debug-io* "   r <expr> - execute and return expression~%" )

  (format  *debug-io* "       # nn - set print depth to nn~%" )
  (format  *debug-io* "       . nn - set print length to nn~%" )

  (format  *debug-io* "          h - print this summary~%" )
  (terpri *debug-io*))
 
 
;evaluate a form in the given environment
(defun step-do-form (f1 env)
  (step-spaces *hooklevel*)
  (princ *hooklevel* *debug-io*)
  (princ " res: " *debug-io*)
  (prin1 (evalhook f1 nil nil env) *debug-io*)) ;print result
 
;set new print depth
(defun step-set-depth (cf)
  (cond ((numberp cf)
         (setq *stepper-depth* (truncate cf)))
        (t (setq *stepper-depth* 3))))
 
;set new print length
(defun step-set-length (cf)
  (cond ((numberp cf)
         (setq *stepper-length* (truncate cf)))
        (t (setq *stepper-length* 3))))
 
;print environment
(defun step-print-env (env)
  (terpri *debug-io*)
  (step-spaces *hooklevel*)
  (princ *hooklevel* *debug-io*)
  (princ " env: " *debug-io*)
  (prin1 env *debug-io*)
  (terpri *debug-io*))
 
;set breakpoints
(defun step-set-breaks (l)
  (cond ((null l) t)
        ((symbolp l) (setq *steplist* (cons l *steplist*)))
        ((listp l)
         (step-set-breaks (car l))
         (step-set-breaks (cdr l)))))
 
;clear breakpoints
(defun step-clear-breaks (l)
  (cond ((null l) t)
        ((eql l '*all*) (setq *steplist* nil))
        ((symbolp l) (delete l *steplist*))
        ((listp l)
         (step-clear-breaks (car l))
         (step-clear-breaks (cdr l)))))
 
;print backtrace
(defun step-baktrace (&aux l n)
  (setq l *callist*
        n *hooklevel*)
  (while (>= n 0)
    (terpri *debug-io*)
    (step-spaces n)
    (prin1 n *debug-io*)
    (princ " " *debug-io*)
    (if (consp (car l)) ;; must handle case where item is list TAA
        (format *debug-io* "~s ~s" (caar l) (cdar l))
        (prin1 (car l) *debug-io*))
    (setq l (cdr l))
    (setq n (1- n)))
  (terpri *debug-io*))
 
;; Added function step-add-level for clarity, since function has
;; become more complex. TAA

(defun step-add-level (form env)
       (setq *callist*  ;; Modified so that callist entry can be
                        ;; list where cadr is a tag saved for later
                        ;; match. This us used for block, return-from,
                        ;; catch, and throw.
             (cons (case (car form)
                         ((block return-from)
                          (cons (car form) (cadr form)))
                         ((catch throw) ;; we may need to eval symbol
                          (if (symbolp (cadr form))
                              (cons (car form) 
                                    (evalhook (cadr form) nil nil env))
                              (if (eq (caadr form) 'quote) ;; quoted tag
                                  (cons (car form) (cadadr form))
                                  nil))) ;; out of luck!
                         (t (car form)))
                   *callist*))) ;add fn. to call list

;; Added function step-prune-level for clarity  TAA

(defun step-prune-level ()
       (setq *hooklevel* (1- *hooklevel*))
       (setq *callist* (cdr *callist*)))

;; Deleted fix-go, replaced with step-fix-levels which handles go, return,
;; and return-from. TAA

(defun step-fix-levels ()
  (cond ((eq (car *callist*) 'go) ;; go -- prune back to tagbody
         (loop
          (when (null *callist*) (return))      ;; we are lost!
          (when (member (car *callist*)
                        '(loop do do* dolist dotimes prog prog* tagbody))
                (return))
          (step-prune-level)))


        ((or (eq (car *callist*) 'return) ;; return -- prune back before block
             (and (consp (car *callist*)) ;; return-from nil is same
                  (eq (caar *callist*) 'return-from) 
                  (null (cdar *callist*))))
         (loop
          (step-prune-level)
          (when (null *callist*) (return))      ;; we are lost!
          (when (member (car *callist*)
                        '(loop do do* dolist dotimes prog prog*))
                (return))))

        ((and (consp (car *callist*)) ;; return-from - prune back before block
              (eq (caar *callist*) 'return-from))
         (let ((target (cdar *callist*)))
              (loop
               (step-prune-level)
               (when (null *callist*) (return)) ;; we are lost!
               (when (or (eq target (car *callist*))
                         (and (consp (car *callist*))
                              (eq (caar *callist*) 'block)
                              (eq (cdar *callist*) target)))
                     (return)))))))

;; Added step-fix-throw TAA

(defun step-fix-throw () ;; fix levels after evalhook for throw
       (when (and (consp (car *callist*))
                  (eq (caar *callist*) 'throw))
             (let ((target (cdar *callist*)))
                  (loop
                   (step-prune-level)
                   (when (null *callist*) (return))     ;; we are lost!
                   (when (and (consp (car *callist*))
                              (eq (caar *callist*) 'catch)
                              (eq (cdar *callist*) target))
                         (return))))))

;;-- Modification MCG 5/5/93

(defun is-brk-in-form (form brklst)
 (prog () 
  (mapcar #'(lambda (x)
              (cond
               ((listp x) (if (is-brk-in-form x brklst) (return t)))
               ((and (or (symbolp x) (functionp x))
		     (member x brklst))
		(return t)))
            )
   form)
   (return nil)))                                         

;; Use this function  for common LISP 
(defun get-key ()
  (let ((val nil))
  (while (or (null val) (eq val #\newline))
         (setq val (read-char))
   ) 
 (char-downcase val)))
 
       
