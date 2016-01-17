;;;;
;;;; DDE Support for XLISP-STAT
;;;; Copyright (c) 1999, by Luke Tierney
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.
;;;;

(in-package "SYSTEM")

;;;;
;;;; Client Functions
;;;;

(export '(dde-request dde-poke dde-execute))

(defun dde-execute (con cmd &key timeout)
  (if timeout
      (dde-client-transaction con :data cmd :timeout timeout)
    (dde-client-transaction con :data cmd)))

(defun dde-request (con item &key binary timeout)
  (if timeout
      (dde-client-transaction con :type :request :item item :binary binary
                              :timeout timeout)
    (dde-client-transaction con :type :request :item item :binary binary)))

(defun dde-poke (con item value &key timeout)
  (let ((vstring (if (stringp value) value (format nil "~s" value))))
    (if timeout
        (dde-client-transaction con :type :poke :item item :data vstring
                                :timeout timeout)
      (dde-client-transaction con :type :poke :item item :data vstring))))

;;**** A little example:
(defun dde-eval (e)
  (let* ((c (dde-connect "XLISP-STAT"))
         (success (if c (dde-execute c (format nil "~s" e)) nil))
         (v (if success (dde-request c "value") nil)))
    (when c (dde-disconnect c))
    (if success
        (read-from-string v)
      (error "evaluation failed"))))


;;;;
;;;; Server Support
;;;;

(defconstant *dde-servers* (make-hash-table :test 'equal))
(defconstant *dde-conversations* (make-hash-table))
(defparameter *dde-debug* nil)

;;**** need to be able to remove service too
(defun dde-add-server (server)
  (let ((service (string-upcase (send server :name)))
        (old (gethash service *dde-servers*)))
    (when (or old (dde-name-service service))
      (setf (gethash service *dde-servers*) server)
      t)))

(defun dde-find-server (name)
  (values (gethash (string-upcase name) *dde-servers*)))

;;**** could use a convention about getting back error info from executes
(defun dde-server-callback (type fmt hconv hsz1 hsz2 data dw1 dw1)
  (dde-debug "Server args: ~s~%" (list type fmt hconv hsz1 hsz2 data dw1 dw1))
  (ignore-errors
   (case type
         (:connect
          (let ((server (gethash hsz2 *dde-servers*)))
            (and server (send server :has-topic hsz1))))
         (:connect-confirm
          (let* ((server (gethash hsz2 *dde-servers*))
                 (conv (send server :make-conversation hsz1)))
            (setf (gethash hconv *dde-conversations*) conv)))
         (:wildconnect
          (let ((val nil))
            (flet ((servs (servname server)
                          (let ((topics (send server :topics)))
                            (dolist (topic topics)
                                    (push (list servname topic) val)))))
                  (maphash #'servs *dde-servers*)
                  val)))
         (t (let ((conv (gethash hconv *dde-conversations*)))
              (case type
                    (:execute (send conv :execute data))
                    (:request (send conv :request hsz2))
                    (:poke (send conv :poke hsz2 data))
                    (:disconnect
                     (remhash hconv *dde-conversations*)
                     (send conv :disconnect))))))))

(defun dde-debug (fmt &rest args)
  (when *dde-debug* (apply #'format *debug-io* fmt args)))


;;;;
;;;; Standard Server
;;;;

(defproto dde-server-proto '(name topics))

(defmeth dde-server-proto :isnew (name)
 (setf (slot-value 'name) name))

(defmeth dde-server-proto :name () (slot-value 'name))
(defmeth dde-server-proto :has-topic (topic)
  (if (assoc topic (slot-value 'topics) :test #'equal) t nil))

(defmeth dde-server-proto :topics ()
  (mapcar #'first (slot-value 'topics)))

(defmeth dde-server-proto :add-topic (topic factory)
  (let* ((topic (string-upcase topic))
         (entry (assoc topic (slot-value 'topics) :test #'equal)))
    (if entry
        (setf (second entry) factory)
      (push (list topic factory) (slot-value 'topics)))))

(defmeth dde-server-proto :make-conversation (topic)
  (let ((confac (second (assoc topic (slot-value 'topics) :test #'equal))))
    (if (objectp confac)
        (send confac :new self topic)
      (funcall confac self topic))))


;;;;
;;;; Standard Conversation
;;;;

(defproto dde-conversation-proto '(server topic value))

(defmeth dde-conversation-proto :isnew (server topic)
  (setf (slot-value 'server) server)
  (setf (slot-value 'topic) topic))

;; Using the following modified readtable allows commands to be
;; enclosed in [...].  This seems to be necessary to properly handle
;; execute transactions sent by Excel.
(defconstant *dde-readtable* (copy-readtable nil))
(set-macro-character #\[ #'(lambda (x y) (values)) t *dde-readtable*)
(set-macro-character #\] #'(lambda (x y) (values)) t *dde-readtable*)

(defmeth dde-conversation-proto :execute (cmd)
  (let ((*readtable* *dde-readtable*)
        (eof (cons nil nil)))
    (with-input-from-string (s cmd)
      (do ((expr (read s nil eof) (read s nil eof)))
          ((eq expr eof))
          (setf (slot-value 'value) (eval expr))))
    t))

(defmeth dde-conversation-proto :request (item)
  (when (equal item "VALUE")
    (format nil "~s" (slot-value 'value))))

(defmeth dde-conversation-proto :poke (item data) nil)

(defmeth dde-conversation-proto :disconnect () nil)


;;;;
;;;; Initialize the Standard Server
;;;;

(let ((server (send dde-server-proto :new "XLISP-STAT")))
 (send server :add-topic "XLISP-STAT" dde-conversation-proto)
 (send server :add-topic "SYSTEM" dde-conversation-proto)
 (setf (gethash "XLISP-STAT" system::*dde-servers*) server))


;;;;
;;;; Command Line Conversation Prototype
;;;;

(defproto cmdline-conversation-proto nil nil dde-conversation-proto)

;;**** get this internally?
(defconstant *banner*
  (format nil "XLISP-PLUS version 3.04~%~
               Portions Copyright (c) 1988, by David Betz.~%~
               Modified by Thomas Almy and others.~%~
               XLISP-STAT Release ~d.~d.~d.~%~
               Copyright (c) 1989-1999, by Luke Tierney.~%"
          xls-major-release
          xls-minor-release
          xls-subminor-release))

(defmeth cmdline-conversation-proto :isnew (server topic)
  (call-next-method server topic)
  (setf (slot-value 'value)
        (format nil "~a~%~a" *banner* (make-prompt-string))))

(defun make-prompt-string ()
  (if (eq (find-package "USER") *package*)
      "> "
    (format nil "~a> " (package-name *package*))))

(defun read-eval-print-from-string (string)
  (with-input-from-string (*standard-input* string)
    (with-output-to-string (*standard-output*)
      (let ((*debug-io* *standard-output*))
        (let ((eof (cons nil nil)))
          (do ((expr (read *standard-input* nil eof)
                     (read *standard-input* nil eof)))
              ((eq expr eof))
              (setf +++ ++ ++ + + - - expr)
              (multiple-value-bind (values error)
                                   (ignore-errors
                                    (multiple-value-list (eval expr)))
                (cond
                 (error (format t "~&Error: ~a~%" error))
                 (t (setf *** ** ** * * (first values))
                    (format t "~{~&~s~%~}" values))))
              (format t "~&~a" (make-prompt-string))))))))

(defmeth cmdline-conversation-proto :execute (cmd)
  (setf (slot-value 'value) (read-eval-print-from-string cmd)))

(defmeth cmdline-conversation-proto :request (item)
  (when (equal item "VALUE")
    (let ((value (slot-value 'value)))
      (setf (slot-value 'value) "")
      value)))


;;;;
;;;; Add Command Line Handler to Server
;;;;

(send (dde-find-server "XLISP-STAT")
      :add-topic "CMDLINE" cmdline-conversation-proto)
