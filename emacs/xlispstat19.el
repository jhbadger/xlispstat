;; Running an inferior xlispstat process under GNU Emacs 19.19

(require 'inf-lisp)

(defun xlisp-quit-sentinel (proc reason)
  (if (and (not (memq reason '(run stop))))
      (save-buffers-kill-emacs)))

(defun set-xlisp-sentinel ()
  (let ((process (inferior-lisp-proc)))
    (set-process-sentinel process 'xlisp-quit-sentinel)
    (process-kill-without-query process)))

(defun run-xlispstat-exit () 
  (run-xlispstat)
  (set-xlisp-sentinel))

(defun run-xlispstat ()
  "Run an inferior xlispstat process."
  (interactive)
  (inferior-lisp "xlispstat"))

(defun run-kcl-exit () 
  (run-kcl)
  (set-xlisp-sentinel))

(defun run-kcl ()
  (interactive)
  (inferior-lisp "kcl"))

