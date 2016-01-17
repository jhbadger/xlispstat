;;; The profile utility, based on Norvig's "Paradigms of Artificial
;;; Intelligence programming".
;;; Adapted for XLisp by Leo Sarasua (modifications marked LSG)


#+:packages
(unless (find-package "TOOLS")
	(make-package "TOOLS" :use '("XLISP")))

(in-package "TOOLS")

(export '(profile unprofile profile-report with-profiling))


(defmacro profile (&rest fn-names)
  "Profile fn-names. With no args, list profiled functions."
  `(mapcar #'profile1
           (setf *profiled-functions*
                 (union *profiled-functions*
                        (remove-if-not #'fboundp ',fn-names) )))) ; LSG


(defmacro unprofile (&rest fn-names)
  "Stop profiling fn-names. With no args, stop all profiling."
  `(progn
      (mapcar #'unprofile1
              ,(if fn-names `',fn-names *profiled-functions*) )
      (setf *profiled-functions*
            ,(if (null fn-names)
                 nil
                 `(set-difference *profiled-functions*
                                  ',fn-names )))))


(defun profile1 (fn-name)
  "Make the function count how often it is called"
  ;; First save away the old, unprofiled function.
  ;; Then make the name be a new function that increments
  ;; a counter and then calls the original function.
  (let ((fn (symbol-function fn-name)))
    (unless (eq fn (get fn-name 'profiled-fn))
      (let ((new-fn (profiled-fn fn-name fn)))
        (setf (symbol-function fn-name) new-fn
              (get fn-name 'profiled-fn) new-fn
              (get fn-name 'unprofiled-fn) fn
              (get fn-name 'profile-time) 0
              (get fn-name 'profile-count) 0 ))))
  fn-name )



(defun unprofile1 (fn-name)
  "Make the function stop counting how often it is called"
  (when (fboundp fn-name)  ; LSG
    (setf (get fn-name 'profile-time) 0)
    (setf (get fn-name 'profile-count) 0)
    (when (eq (symbol-function fn-name) (get fn-name 'profiled-fn))
       ;; normal case: restore unprofiled version
       (setf (symbol-function fn-name)
             (get fn-name 'unprofiled-fn) ))
    fn-name ))


(defun profile-report (&optional
                       (fn-names (copy-list *profiled-functions*))
                       (key #'profile-count))
  "Report profiling statistics on given functions."
  (let ((total-time (reduce #'+ (mapcar #'profile-time fn-names))))
    (unless (null key)
      (setf fn-names (sort fn-names #'> :key key)) )
    (format t "~&Total elapsed time: ~d seconds."
            (fast-time->seconds total-time) )
    (format t "~&  Count     Secs Time% Name")
    (dolist (name fn-names)
       (format t "~&~7D   ~6,2F  ~3d% ~A"
               (profile-count name)
               (fast-time->seconds (profile-time name))
               (if (< total-time 1e-9)
                   0
                   (round (/ (profile-time name) total-time) .01) )
               name ))))


(defmacro with-profiling (fn-names &rest body)
  `(progn
     (unprofile . ,fn-names)
     (profile . ,fn-names)
     (setf *profile-call-stack* nil)
     (unwind-protect
       (progn . ,body)
       (profile-report ',fn-names)
       (unprofile . ,fn-names) )))


(defun profiled-fn (fn-name fn)
  "Return a function that increments the count, and times."
  #'(lambda (&rest args)
      (profile-enter fn-name)
      (multiple-value-prog1
         (apply fn args)
         (profile-exit fn-name) )))

(defun profile-count (fn-name) (get fn-name 'profile-count))

(defun profile-time (fn-name) (get fn-name 'profile-time))


(defvar *profiled-functions* nil
  "Function names that are currently profiled" )

(defvar *profile-call-stack* nil)


(defun profile-enter (fn-name)
   (incf (get fn-name 'profile-count))
   (unless (null *profile-call-stack*)
     ;; Time charged against the calling function:
     (inc-profile-time (first *profile-call-stack*)
                       (car (first *profile-call-stack*)) ))
   ;; Put a new entry on the stack
   (push (cons fn-name (get-fast-time))
         *profile-call-stack* ))


(defun profile-exit (fn-name)
  ;; Time charged against the current function:
  (inc-profile-time (pop *profile-call-stack*)
                    fn-name )
  ;; Change the top entry to reflect current time
  (unless (null *profile-call-stack*)
    (setf (cdr (first *profile-call-stack*))
          (get-fast-time) )))


(defun inc-profile-time (entry fn-name)
  (incf (get fn-name 'profile-time)
        (fast-time-difference (get-fast-time) (cdr entry)) ))

(defun fast-time->seconds (time)
  "Convert a fast time interval into seconds"
  (float (/ time internal-time-units-per-second)) )


(defun get-fast-time ()
  "Return the elapsed time. This may wrap around;
  use FAST-TIME-DIFFERENCE to compare."
  (get-internal-real-time) )

(defun fast-time-difference (end start)
  "Subtract two time points."
  (- end start) )

