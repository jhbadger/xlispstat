; initialization file for XLISP-PLUS 3.03

(princ "XLISP-PLUS 3.03 contains contributed code by:
Tom Almy, Mikael Pettersson, Neal Holtz, Johnny Greenblatt, Ken Whedbee,
Blake McBride, Pete Yadlowsky, Hume Smith, and Richard Zidlicky.
Portions copyright (c) 1988, Luke Tierney.\n")

;; Set this up however you want it
(setq *features* (list :xlisp :21h :v30 :v303))

;; Differences in various implementations, needed by example programs
(when (fboundp 'export)
      (setq *features* (cons :packages *features*)))
#+:packages
(in-package "XLISP")
(when (fboundp 'get-internal-run-time) 
      (setq *features* (cons :times *features*)))
(when (fboundp 'generic) 
      (setq *features* (cons :generic *features*)))
(when (fboundp 'find-if)
      (setq *features* (cons :posfcns *features*)))
(when (fboundp 'numerator)
      (setq *features* (cons :bignums *features*)))
(when (fboundp 'log)
      (setq *features* (cons :math *features*)))
(when (alphanumericp #\M-C-@)
      (setq *features* (cons :pc8 *features*)))
(when (fboundp 'values)
      (setq *features* (cons :mulvals *features*)))
(when (fboundp 'get-key)
      (setq *features* (cons :getkey *features*)))

#-:packages
(defun export (x &optional y) t) ;; dummy definitions for package functions
#-:packages
(defun in-package (x))

(export '(strcat set-macro-character get-macro-character savefun
	  debug nodebug classp))

(defun strcat (&rest str)	;; Backwards compatibility
       (apply #'concatenate 'string str))


; (set-macro-character ch fun [ tflag ])
(defun set-macro-character (ch fun &optional tflag)
    (setf (aref *readtable* (char-int ch))
          (cons (if tflag :tmacro :nmacro) fun))
    t)

; (get-macro-character ch)
(defun get-macro-character (ch)
  (if (consp (aref *readtable* (char-int ch)))
    (cdr (aref *readtable* (char-int ch)))
    nil))

; (savefun fun) - save a function definition to a file
(defmacro savefun (fun)
  `(let* ((fname (strcat (symbol-name ',fun) ".lsp"))
          (fval (get-lambda-expression (symbol-function ',fun)))
          (fp (open fname :direction :output)))
     (cond (fp (print (cons (if (eq (car fval) 'lambda)
                                'defun
                                'defmacro)
                            (cons ',fun (cdr fval))) fp)
               (close fp)
               fname)
           (t nil))))

; (debug) - enable debug breaks
(defun debug ()
       (setq *breakenable* t))

; (nodebug) - disable debug breaks
(defun nodebug ()
       (setq *breakenable* nil))

; (classp) - Class predicate (was defined in classes.lsp)
(defun classp (name)
       (when (objectp name)
	     (eq (send name :class) class)))


; initialize to enable breaks but no trace back
(setq *breakenable* t *tracenable* nil)


; macros get displaced with expansion
; Good feature, but comment out to avoid shock.
(setq *displace-macros* t)

;; Select one of these three choices
;; Other modes will not read in other standard lsp files


; print in upper case, case insensitive input
;(setq *print-case* :upcase *readtable-case* :upcase)

; print in lower case
(setq *print-case* :downcase *readtable-case* :upcase)

; case sensitive, lowercase and uppercase swapped (favors lower case)
;(setq *print-case* :downcase *readtable-case* :invert)

; Make this "T" to use doskey or run under Epsilon
; Comment out altogether for non-MSDOS environments
(setq *dos-input* nil)

;; Define Class and Object to be class and object when in case sensitive
;; mode

(when (eq *readtable-case* :invert)
      (defconstant Class class)
      (defconstant Object object)
      (export '(Class Object)))

