#+:packages
(unless (find-package "TOOLS")
	(make-package "TOOLS" :use '("XLISP")))

(in-package "TOOLS")

(export '(edit))

;;;
;;; This variable is the default file to edit
;;;

(defvar *edit-file* "")

(defvar *editor* "epsilon")

;;;
;;; edit a file using the specified editor
;;; if the file editted was a lisp file (.lsp) load it
;;;

;; Two versions, the first works when position-if exists and does a better
;; job   

#+:posfcns (defmacro edit (&optional file &aux rfile)
  (read-char)
  (when file (setq *edit-file* (string file)))
  (setq rfile (reverse *edit-file*))
  (when (null (position-if #'(lambda (x) (eq x #\.))
			   rfile
			   :end 
			   (position-if #'(lambda (x) 
						  (or (eq x #\\) (eq x #\/)))
					rfile)))
	(setq *edit-file* (strcat *edit-file* ".lsp")))
  (unless (system (strcat *editor* " " *edit-file*))
	  (error "Unable to execute: ~a ~a" *editor* *edit-file*))
  (let ((len (length *edit-file*)))
       (when (and (> len 4)
		  (string= (string-downcase (subseq *edit-file* (- len 4)))
			   ".lsp"))
	     (list 'load *edit-file*))))

#-:posfcns (defmacro edit (&optional file)
    (read-char)
    (when file (setq *edit-file* (string file)))
    (when (not (member #\.
		       (get-output-stream-list
			  (make-string-input-stream *edit-file*))))
	  (setq *edit-file* (strcat *edit-file* ".lsp")))
    (unless (system (strcat *editor* " " *edit-file*))
          (error "Unable to execute: ~a ~a" *editor* *edit-file*))
    (let ((len (length *edit-file*)))
      (when (and (> len 4)
		 (string= (string-downcase (subseq *edit-file* (- len 4)))
			  ".lsp"))
	    (list 'load *edit-file*))))
