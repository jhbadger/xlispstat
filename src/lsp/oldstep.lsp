;;;; XLISP-STAT 2.1 Copyright (c) 1990, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.

(defun step (expr)	
  (let ((hooklevel 0)
        (option nil)
        #+macintosh (dialog (step-dialog))
        (help-string "~%:b - break~%:h - help (this message)~%:n - next~%:s - skip~%:e - evaluate~%"))
    (labels ((indent () (terpri) (dotimes (i (* 2 hooklevel)) (princ " ")))
             (read-option (env) 
                          (loop (princ " ? ")
                                (let ((c (read)))
                                  (cond 
                                    ((member c '(:s :n :b)) (return c))
                                    ((equal c :h) (format t help-string))
                                    ((equal c :e)
                                     (print (evalhook (read) 
                                                      nil 
                                                      nil 
                                                      env)))))))
             (trace-hook-function	(expr &optional env) 	
               (setq hooklevel (1+ hooklevel))
               (indent)
               (format t	"Form:   ~s" expr)
               (force-output)
               (let ((value (evalhook expr
                                      #'trace-hook-function
                                      nil
                                      env)))
                 (indent)
                 (format t	"Value:  ~s" value)
                 (force-output)
                 (setq hooklevel (1- hooklevel))
                 value))
             (step-hook-function (expr &optional env) 	
               (setq hooklevel (1+ hooklevel))
               (indent)
               (format t	"Form:   ~s" expr)
               (force-output)
               (setq option (if (atom expr) nil (read-option env)))
               (if (equal option :b) (break)) 
               (let ((value (evalhook expr
                                      (if (equal option :s)
                                          nil ;#'trace-hook-function
                                          #'step-hook-function)
                                      nil
                                      env)))
                 (indent)
                 (format t	"Value:  ~s" value)
                 (force-output)
                 (setq hooklevel (1- hooklevel))
                 value)))
    (unwind-protect (step-hook-function expr)
                    (terpri)
#+macintosh         (send dialog :remove)))))

(defun step-dialog ()
  (let* ((text-item (send text-item-proto :new "                           "
                           :editable t)))
    (send dialog-proto :new
          (list text-item
                (send button-item-proto :new "Eval"
                      :action
                      #'(lambda ()
                          (send *listener* :paste-string
                                (format nil ":e ~s~%" 
                                        (send text-item :text)))))
                (send button-item-proto :new "Next"
                      :action
                      #'(lambda ()
                          (send *listener* :paste-string
                                (format nil ":n~%"))))
                (send button-item-proto :new "Skip"
                      :action
                      #'(lambda ()
                          (send *listener* :paste-string
                                (format nil ":s~%")))))
          :type 'modeless)))
