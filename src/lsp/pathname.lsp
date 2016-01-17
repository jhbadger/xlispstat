;;;;
;;;; Simplified version of some pathname functions.
;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(in-package "XLISP")

(export '(namestring file-namestring directory-namestring
	 pathname pathname-host pathname-device pathname-version
	 parse-namestring pathname-directory pathname-name pathname-type
	 *default-pathname-defaults*
	 make-pathname merge-pathnames))

(defun namestring (x)
  (cond
   ((stringp x) x)
   ((streamp x) (truename x))
   (t (error "bad namestring - ~s" x))))

(defun file-namestring (pathname)
  (make-pathname :name (pathname-name pathname)
                 :type (pathname-type pathname)))

(defun directory-namestring (pathname)
  (make-pathname :directory (pathname-directory pathname)))

(defun parse-namestring (x &optional host defaults &key
			   (start 0)
			   end
			   junk-allowed)
  (cond
   ((stringp x) (subseq x start end))
   ((streamp x) (truename x))
   (junk-allowed nil)
   (t (error "bad namestring - ~s" x))))

(defun pathname (x) (namestring x))

(defun pathname-host (x) nil)
(defun pathname-version (x) nil)

(defvar *default-pathname-defaults* "")

#+(or unix macintosh)
(defun pathname-device (x) (declare (ignore x)) nil)
#+msdos
(defun pathname-device (x)
  (let ((d (position #\: x)))
    (if (and d (= d 1))
        (subseq x 0 1))))

(defconstant *wild-pathname-string*
  #+macintosh "*" ;; this isn't a good choice, but Mac CL uses it too.
  #-macintosh "*")

(defconstant *back-pathname-string*
  #+macintosh "::"
  #-macintosh "..")

(defun simplify-directory (dir)
  (cond
   ((null dir) dir)
   ((and (stringp (first dir)) (eq (second dir) :back))
    (simplify-directory (rest (rest dir))))
   (t (let ((ntail (simplify-directory (rest dir))))
	(if (eq ntail (rest dir))
	    dir
	  (cons (first dir) ntail))))))

;;**** should try to handle :back, :up, and OS equivalents (.., ::, etc.)
(defun make-pathname (&key (defaults "")
			   (host (pathname-host defaults))
			   (device (pathname-device defaults))
			   (directory (pathname-directory defaults))
			   (name (pathname-name defaults))
			   (type (pathname-type defaults))
			   (version (pathname-version defaults)))
  (when (stringp directory)
	(setf directory
	      (list :absolute
		    (string-trim #+unix "/" #+msdos "\\" #+macintosh ":"
				 directory))))
  (let ((nlist nil))
    #+msdos
    (when device (push (concatenate 'string device ":") nlist))
    (setf directory (subst *wild-pathname-string* :wild directory))
    (setf directory (simplify-directory directory))
    (setf directory (subst *back-pathname-string* :back directory))
    (setf directory (subst *back-pathname-string* :up directory))
    (when (eq name :wild) (setf name *wild-pathname-string*))
    (when (eq type :wild) (setf name *wild-pathname-string*))
    (cond
     ((eq (first directory) :absolute)
      #+unix (push "/" nlist)
      #+msdos (push "\\" nlist)
      (pop directory))
     ((eq (first directory) :relative)
      #+macintosh (push ":" nlist)
      (pop directory))
     ((null directory)
      #+macintosh (push ":" nlist)
      #+(or unix msdos) nil)
     (t (error "bad directory - ~s" directory)))
    (dolist (d directory)
      (push (if d (string d)) nlist)
      (push #+unix "/" #+macintosh ":" #+msdos "\\" nlist))
    (push (if name (string name)) nlist)
    (when type
	  (push "." nlist)
	  (push (if type (string type)) nlist))
    (apply #'concatenate 'string (nreverse nlist))))

(defun pathname-directory (x)
  (let ((dir nil)
	(pos 0))
     #+msdos
     (let ((d (position #\: x)))
       (if (and d (= d 1))
           (setf x (subseq x 2))))
     #+(or unix msdos)
     (cond
      ((and (< 0 (length x)) (char= (elt x 0) #+ unix #\/ #+msdos #\\))
       (setf pos 1)
       (push :absolute dir))
      (t (push :relative dir)))
     #+macintosh
     (cond
      ((and (< 0 (length x))
            (find #\: x :test #'char=)
            (not (char= #\: (elt x 0))))
       (push :absolute dir))
      (t (if (and (< 0 (length x)) (char= #\: (elt x 0))) (setf pos 1))
	 (push :relative dir)))
    (loop
     (let ((npos (position #+unix #\/ #+macintosh #\: #+msdos #\\ x :start pos)))
       (unless npos (return (nreverse dir)))
       (push (subseq x pos npos) dir)
       (setf pos (1+ npos))))))

(defun pathname-name (x)
  #+msdos
  (let ((d (position #\: x)))
    (if (and d (= d 1))
        (setf x (subseq x 2))))
  (let* ((start (position #+unix #\/ #+macintosh #\: #+msdos #\\ x :from-end t))
	 (end (position #\. x :from-end t :start (if start start 0)))
	 (name (subseq x (if start (1+ start) 0) end)))
    (if (< 0 (length name)) name)))

(defun pathname-type (x)
  (let* ((start (position #+unix #\/ #+macintosh #\: #+msdos #\\ x :from-end t))
	 (pos (position #\. x :from-end t :start (if start start 0)))
	 (typestr (if pos (subseq x (1+ pos)) "")))
    (if (< 0 (length typestr)) typestr)))

(defun merge-pathnames (path &optional (defaults "") version)
  (declare (ignore version))
  (let ((dir (pathname-directory path))
	(name (pathname-name path))
	(type (pathname-type path))
	(device (pathname-device path)))
    (make-pathname :directory (if dir
				  (if (and (consp dir)
					   (eq (first dir) :relative))
				      (append (pathname-directory defaults)
					      (rest dir))
				      dir)
				  (pathname-directory defaults))
		   :name (if name name (pathname-name defaults))
		   :type (if type type (pathname-type defaults))
                   :device (if device device (pathname-device defaults)))))

;;;;
;;;;
;;;; Replacement for builtin LOAD
;;;;
;;;;

(export '(*load-print* *load-verbose* *load-pathname*
	  *load-pathname-defaults* *load-truename*))

(defvar *load-print* nil)
(defvar *load-verbose* t)
(defvar *load-pathname*)
(defvar *load-truename*)

(defvar *load-pathname-defaults* 
  nil 
  "list of additional defaults for load to try")

(defun do-load (path vflag pflag)
  (let ((*readtable* *readtable*)
	(*package* *package*)
	(*load-pathname* path)
	(*load-truename* (truename path))
	(eof (cons nil nil)))
     (with-open-file (stream path :if-does-not-exist nil)
       (when vflag (format t "~&; loading ~a~%" path))
       (loop
	(let ((expr (read stream nil eof)))
	  (when (eq expr eof) (return t))
	  (let ((val (eval expr)))
	    (when pflag (print val))))))))

(defun do-does-not-exist (file flag)
  (if flag (error "can't load file -- ~s" file)))

(defun load (file &key
		  (verbose *load-verbose*)
		  (print *load-print*)
		  (if-does-not-exist t))
  (let ((load-path (cons *default-pathname-defaults*
			 *load-pathname-defaults*)))
    (cond
     ((pathname-type file)
      (dolist (d load-path (do-does-not-exist file if-does-not-exist))
        (let ((path (merge-pathnames file d)))
	  (if (probe-file path)
	      (return (do-load path verbose print))))))
     (t
      (let ((lspfile (merge-pathnames file ".lsp"))
	    (fslfile (merge-pathnames file ".fsl")))
	(dolist (d load-path (do-does-not-exist file if-does-not-exist))
          (let* ((lsppath (merge-pathnames lspfile d))
		 (fslpath (merge-pathnames fslfile d))
		 (lsp-exists (probe-file lsppath))
		 (fsl-exists (probe-file fslpath)))
	    (cond
	     ((or (and fsl-exists
		       lsp-exists
		       (< (file-write-date lsppath) (file-write-date fslpath)))
		  (and fsl-exists (not lsp-exists)))
	      (return (do-load fslpath verbose print)))
	     (lsp-exists (return (do-load lsppath verbose print)))))))))))

(export '(wild-pathname-p pathname-match-p directory))

;;**** very minimal and very inefficient versions that are just adequate
;;**** to support DIRECTORY
(defun wild-pathname-p (pathname)
  (or (equal (pathname-name pathname) *wild-pathname-string*)
      (equal (pathname-type pathname) *wild-pathname-string*)
      (member *wild-pathname-string* (pathname-directory pathname)
	      :test #'equal)))

(defun pathname-match-p (pathname wildname)
  (and (equal (pathname-directory pathname) (pathname-directory wildname))
       (let ((wname (pathname-name wildname))
	     (wtype (pathname-type wildname))
	     (pname (pathname-name pathname))
	     (ptype (pathname-type pathname)))
	 (if (wild-pathname-p wname)
	     (or (null wtype)
		 (equal ptype wtype)
		 (and ptype (wild-pathname-p wtype)))
	   (and (equal pname wname)
		(or (equal ptype wtype)
		    (and ptype (wild-pathname-p wtype))))))))

;;**** needs to support wild cards in directories and  full pathname matching
(defun directory (arg &key all)
  (let* ((pattern (if (stringp arg) arg (truename arg)))
	 (dir (pathname-directory pattern))
	 (dev (pathname-device pattern))
	 (name (pathname-name pattern))
	 (type (pathname-type pattern))
	 (leafpat (make-pathname :name name :type type)))
    (let* ((dirname (make-pathname :directory dir :device dev))
	   (dirtruename (truename dirname))
	   (dlist (system::base-directory dirtruename))
	   (ndlist (remove-if-not #'(lambda (x) (pathname-match-p x leafpat))
				  dlist))
	   (tdlist (mapcar #'(lambda (x) (merge-pathnames x dirtruename))
			   ndlist)))
      (if all
	  tdlist
	(remove-if-not #'(lambda (x) (eq (system::file-type x) :regular))
		       tdlist)))))


;;;
;;; Autoloading
;;;

(in-package "SYSTEM")
(export '(define-autoload-module register-autoloads
          create-autoload-path))

(defun autoload-function (name)
  (let ((modpath (find-function-module-path name))
        (restart (find-restart 'continue))
        (*load-verbose* nil))
    (when (and modpath restart)
          (load modpath)
          (when (fboundp name)
                (invoke-restart restart)))))

(defun autoload-variable (name)
  (let ((modpath (find-variable-module-path name))
        (restart (find-restart 'continue))
        (*load-verbose* nil))
    (when (and modpath restart)
          (load modpath)
          (when (boundp name)
                (invoke-restart restart)))))

(let ((function-modules (make-hash-table))
      (variable-modules (make-hash-table)))
  (defun find-function-module-path (name)
    (gethash name function-modules))
  (defun find-variable-module-path (name)
    (gethash name variable-modules))
  (defun add-function-module (name module)
    (setf (gethash name function-modules) module))
  (defun add-variable-module (name module)
    (setf (gethash name variable-modules) module)))

(defmacro define-autoload-module (module &rest clauses)
  `(let ((mname (make-pathname :name ',module
                               :directory (pathname-directory *load-truename*)
                               :device (pathname-device *load-truename*)
                               :host (pathname-host *load-truename*)))
         (clist ',clauses))
     (dolist (c clist)
       (ecase (first c)
         (variable (dolist (n (rest c)) (add-variable-module n mname)))
         (function (dolist (n (rest c)) (add-function-module n mname)))))))

(defun register-autoloads (dir)
  (let ((idx (merge-pathnames "_autoidx" dir))
        (dirlist (system::base-directory dir)))
    #+(or unix msdos) (setf dirlist (delete "." dirlist :test #'equal))
    #+(or unix msdos) (setf dirlist (delete ".." dirlist :test #'equal))
    (load idx :verbose nil :if-does-not-exist nil)
    (dolist (d dirlist)
      (let ((dpath (make-pathname :directory (list :relative d))))
        (register-autoloads (merge-pathnames dpath dir))))))

(defun create-autoload-path ()
  (list (merge-pathnames (make-pathname :directory '(:relative "Autoload"))
                         *default-path*)))
