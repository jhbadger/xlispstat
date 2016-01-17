;;;;
;;;; help.lsp XLISP-STAT help functions
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(in-package "XLISP")

(provide "help")

(export '(help help*))

;;;;
;;;; Help Functions
;;;;
     
(defun help (&optional s)
"Args: (&optional symbol)
Prints the documentation associated with SYMBOL.  With no argument, 
this function prints the greeting message to beginners."
  (cond 
    ((null s) (princ "***Intro not yet available***"))
    (t (let ((docf (documentation s 'function))
             (docv (documentation s 'variable))
             (doct (documentation s 'type))
             (docs (documentation s 'setf)))
         (unless (or docf docv doct docs)
                 (format t "Sorry, no help available on ~a~%" s))
         (flet ((put-doc (sym type str)
                         (princ sym)
                         (dotimes (i (- *line-length*
                                        (length (string sym))
                                        (length (string type))))
                                  (princ " "))
                         (princ type)
                         (terpri)
                         (princ str)
                         (terpri)))
           (if docf (put-doc s "[function-doc]" docf))
           (if docv (put-doc s "[variable-doc]" docv))
           (if doct (put-doc s "[type-doc]" doct))
           (if docs (put-doc s "[setf-doc]" docs))))))
  nil)

(defun help* (sl)
"Args: (string)
Prints the documentation associated with those symbols whose print names
contain STRING as substring.  STRING may be a symbol, in which case the
print-name of that symbol is used."
  (dotimes (i *line-length*) (princ "-"))
  (terpri)
  (dolist (s (mapcar #'intern 
                     (sort-data (mapcar #'string (apropos-list sl)))))
          (help s)
          (dotimes (i *line-length*) (princ "-"))
          (terpri)))

;;;;
;;;; Object Help Stuff
;;;;

(defmeth *object* :doc-topics ()
"Method args: ()
Returns all topics with documentation for this object."
  (load-help)
  (remove-duplicates 
   (mapcar #'car 
           (apply #'append 
                  (mapcar 
                   #'(lambda (x) 
                       (if (send x :has-slot 'documentation :own t)
                           (send x :slot-value (quote documentation))))
                   (send self :precedence-list))))))

(defmeth *object* :documentation (topic &optional (val nil set))
"Method args: (topic &optional val)
Retrieves or sets object documentation for topic."
  (unless set (load-help))
  (if set (send self :internal-doc topic val))
  (let ((val (dolist (i (send self :precedence-list))
                     (let ((val (send i :internal-doc topic))) 
                       (if val (return val))))))
    (when (and (numberp val) (streamp *help-stream*))
          (file-position *help-stream* val)
          (setq val (read *help-stream*)))
    val))

(defmeth *object* :delete-documentation (topic)
"Method args: (topic)
Deletes object documentation for TOPIC."
  (setf (slot-value 'documentation)
        (remove :title nil :test #'(lambda (x y) (eql x (first y)))))
  nil)

(defmeth *object* :help (&optional topic)
"Method args: (&optional topic)
Prints help message for TOPIC, or general help if TOPIC is NIL."
  (if topic 
      (let ((doc (send self :documentation topic)))
        (cond 
          (doc (princ topic) (terpri) (princ doc) (terpri))
          (t (format t "Sorry, no help available on ~a~%" topic))))
      (let ((topics (sort-data (mapcar #'string (send self :doc-topics))))
            (proto-doc (send self :documentation 'proto)))
        (if (send self :has-slot 'proto-name)
	    (format t "~s~%" (slot-value 'proto-name)))
        (when proto-doc (princ proto-doc) (terpri))
        (format t "Help is available on the following:~%~%")
        (dolist (i topics) (princ i) (princ " "))
        (terpri))))
