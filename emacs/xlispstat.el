;; Running an inferior xlispstat process
;; Copyright (C) 1985, 1986, 1987, 1988 Free Software Foundation, Inc.

;; This file is part of GNU Emacs.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY.  No author or distributor
;; accepts responsibility to anyone for the consequences of using it
;; or for whether it serves any particular purpose or works at all,
;; unless he says so in writing.  Refer to the GNU Emacs General Public
;; License for full details.

;; Everyone is granted permission to copy, modify and redistribute
;; GNU Emacs, but only under the conditions described in the
;; GNU Emacs General Public License.   A copy of this license is
;; supposed to have been given to you along with GNU Emacs so you
;; can know your rights and responsibilities.  It should be in a
;; file named COPYING.  Among other things, the copyright notice
;; and this notice must be preserved on all copies.

(require 'shell)

(defun xlisp-quit-sentinel (proc reason)
  (if (and (not (memq reason '(run stop))))
      (save-buffers-kill-emacs)))

(defun set-xlisp-sentinel ()
  (let ((process (get-process "lisp")))
    (set-process-sentinel process 'xlisp-quit-sentinel)
    (process-kill-without-query process)))

(defun run-xlispstat-exit () 
  (run-xlispstat)
  (set-xlisp-sentinel))

(defun run-kcl-exit () 
  (run-kcl)
  (set-xlisp-sentinel))

(defun run-xlispstat ()
  "Run an inferior xlispstat process."
  (interactive)
  (let ((old-prog inferior-lisp-program))
    (setq inferior-lisp-program "xlispstat")
    (run-lisp)
    (setq inferior-lisp-program old-prog)))

(defun run-kcl ()
  (interactive)
  (let ((old-prog inferior-lisp-program))
    (setq inferior-lisp-program "kcl")
    (run-lisp)
    (setq inferior-lisp-program old-prog)))

;; modified to use .lsp suffix on temp file - xlisp gets upset without it.
(defun lisp-send-defun (display-flag)
  "Send the current defun to the Lisp process made by M-x run-lisp.
With argument, force redisplay and scrolling of the *lisp* buffer.
Variable `inferior-lisp-load-command' controls formatting of
the `load' form that is set to the Lisp process."
  (interactive "P")
  (or (get-process "lisp")
      (error "No current lisp process"))
  (save-excursion
   (end-of-defun)
   (let ((end (point))
	 (filename (format "/tmp/emlisp%d.lsp" (process-id (get-process "lisp")))))
     (beginning-of-defun)
     (write-region (point) end filename nil 'nomessage)
     (process-send-string "lisp" (format inferior-lisp-load-command filename)))
   (if display-flag
       (let* ((process (get-process "lisp"))
	      (buffer (process-buffer process))
	      (w (or (get-buffer-window buffer) (display-buffer buffer)))
	      (height (window-height w))
	      (end))
	 (save-excursion
	   (set-buffer buffer)
	   (setq end (point-max))
	   (while (progn
		    (accept-process-output process)
		    (goto-char (point-max))
		    (beginning-of-line)
		    (or (= (point-max) end)
			(not (looking-at inferior-lisp-prompt)))))
	   (setq end (point-max))
	   (vertical-motion (- 4 height))
	   (set-window-start w (point)))
	 (set-window-point w end)))))
