; This is an XLISP-PLUS glossary lookup package.
; It requires the package facility to work, and uses a file called
; glos.txt which is the glossary portion of the XLISP documentation file
; When loaded for the first time, it adds a *glossary* property to all
; functions which are defined in glos.txt and are in the XLISP package.
; This property is the displacement into the file. When a glossary lookup
; occurs the file itself is referenced. By operating this way, very little
; space is taken for this feature.

; There are two user-accessable symbols. tools:*glospaging* is a variable
; which causes the output to "page" (pause for user response) at every
; screenful. Set it to NIL to defeat this feature or to the number of lines
; per page to enable.

; The main entry point is the function tools:glos. When given an argument that
; is a function symbol, it will look up the glossary definition. If the
; symbol is not in the XLISP package, or if a second non-nil argument is
; supplied, the name will be passed to APROPOS, and the glossary definitions
; for all matching symbols will be displayed

; For instance (glos :car) or (glos 'car) or (glos "car") will show the
; definition for the CAR function, while (glos 'car t) will show that of
; MAPCAR as well. (glos "X") will give the glossary listing of all functions
; with names containing an X character, since there is no external symbol
; named X in the XLISP package.

; It would not be that difficult to modifify this program for environments
; where packages are not compiled in, however operation would not be quite
; as sophisticated.

;Tom Almy
;10/93

; Revised 2/94, improving operation and clarifying some loading messages

; Revised 10/14/96 to create file useable with function DOCUMENTATION


#-:packages
(error "This utility was written asuming the package facility is in use")
#-:common
(load "common")

(unless (find-package "TOOLS")
	(make-package "TOOLS" :use '("XLISP")))

(in-package "TOOLS")

(export '(glos *glospaging*))

(defvar *glosfilename*)

; We will look things up while loading
; so we can toss all the code when done

(unless *glosfilename*
	(format t "~&Building glossary references---")
	(let ((lpar #\()
	      (rpar #\))
	      (dot #\.)
	      (*pos* 0)
	      symbol)
	     (labels (
	      

(xposition (chr str &aux (pos (position chr str)))
       (if pos pos (length str)))

(seek-next-fcn (strm)
       (do ((thispos *pos* (file-position strm))
	    (text (read-line strm nil) (read-line strm nil)))
	   ((null text) nil)
	   (when (and (> (length text) 3)
		      (or (char= lpar (char text 0))
			  (char= dot (char text 0))))
		 (setf *pos* thispos)
		 (return-from seek-next-fcn
			      (subseq text 1 (min (xposition rpar text)
						  (xposition #\space text))))))))

;; The body of the code that does the work:		   
		   (unless (open "glos.txt" :direction :probe)
			   (error "Could not find glossary file glos.txt"))
		   (with-open-file
		    (strm "glos.txt")
		    (setq *glosfilename* (truename strm))
		    (do ((name (seek-next-fcn strm) (seek-next-fcn strm)))
			((null name) nil)
			(setq symbol (find-symbol (string-upcase name) :xlisp))
			(unless symbol
				(if (string-equal name "nil")
				    (setf (get nil '*glossary*) *pos*)
				    (format t
					    "~&Documented symbol ~s not found in XLISP.~%"
					    name)))
			(when symbol
;			      (format t "~s " symbol)
			      (setf (get symbol '*glossary*) *pos*))))
;; Check for functions & vars in package XLISP that aren't documented
		   (format t "~&Not documented, but found in XLISP:")
		   (do-external-symbols
		    (x :xlisp)
		    (when (and (or (fboundp x) (specialp x))
			       (not (get x '*glossary*)))
			  (format t "~s " x)))
		   (format t "~&")

))) ;; Ends the Flet, let, and unless

(defvar *linecount*)
(defvar *glospaging* 23)

(defun linechk ()
       (when (and *glospaging*
		  (> (incf *linecount*) *glospaging*))
	     (setq *linecount* 0)
	     (if (y-or-n-p "--PAUSED--  Continue?")
		 (fresh-line)
		 (throw 'getoutahere))))
       
(defun glos2 (val)
       (with-open-file
	(strm *glosfilename*)
	(file-position strm val)
	(do ((line (read-line strm nil) (read-line strm nil)))
	    ((zerop (length line))
	     (linechk)
	     (format t "~%"))
	    (linechk)
	    (format t "~a~%" line))))


(defun glos (symbol &optional matchall &aux val (sym (string symbol)))
       (catch
	'getoutahere
	(setq *linecount* 0)
	(if (and (null matchall) (setq val (find-symbol sym)))
	    (if (setq val (get val '*glossary*))
		(glos2 val)
		(format t"No information on ~a~%" sym))
	    (progn
	     (setq val
		   (do ((list (apropos-list sym :xlisp) (cdr list))
			(result nil result))
		       ((null list) result)
		       (when (setq val (get (car list) '*glossary*))
			     (pushnew val result))))
	     (if (zerop (length val))
		 (format t "No matches for ~a~%" symbol)
		 (map nil #'glos2 val)))))
#+:mulvals (values)
#-:mulvals nil
)
