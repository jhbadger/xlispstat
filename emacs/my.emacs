;; functions for running an inferior xlispstat process

(autoload 'run-xlispstat "/usr/sun1/luke/lib/xlispstat" 
	  "Run inferior xlispstat process" t)
(autoload 'run-kcl "/usr/sun1/luke/lib/xlispstat" 
	  "Run inferior kcl process" t)

(global-set-key "\C-xg" 'goto-line)
(global-set-key "\C-xd" 'lisp-send-defun)
(global-set-key "\C-xj" 'lisp-send-defun-and-go)
