;;;;
;;;; This file can be used to make .fsl files from .lsp files.
;;;;

(defpackage "XLSCMP"
  (:use "XLISP")
  (:import-from "XLISP" "*CMP-SETF*" "ADD-METHOD" "CMP-DO-DEFSTRUCT"))

(require "cmpload")

(flet ((compile-if-needed (file &optional load)
         (let ((lspfile (merge-pathnames file ".lsp"))
               (fslfile (merge-pathnames file ".fsl")))
           (unless (and (probe-file lspfile)
                        (probe-file fslfile)
                        (< (file-write-date lspfile)
                           (file-write-date fslfile)))
                   (compile-file file :load load)))))
  (let ((stdfiles '("common" "common2" "common3" "pathname" "help" "objects"
		    "conditns" "shlib" "loadfsl"))
        (otherfiles '("init" "cmpload" "linalg" "stats" "dialogs" "graphics"
		      "graph2" "graph3" "regress" "menus" "oneway" "nonlin"
                      "maximize" "bayes" "stepper" "glim"
		      #+msdos "dde"))
        (cmpfiles '("backquot" "cmpmacro" "convert" "cells" "simplify"
                    "lift" "gencode" "peephole" "assemble" "cmpfront")))
    (dolist (f cmpfiles)
      (let ((fn (make-pathname :name f :directory '(:relative "compiler"))))
        (compile-if-needed fn t)))
    (dolist (f stdfiles) (compile-if-needed f t))
    (dolist (f otherfiles) (compile-if-needed f))))

(exit)
