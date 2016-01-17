(defpackage "XLSCMP"
  (:use "XLISP")
  (:import-from "XLISP" "*CMP-SETF*" "*CMP-STRUCTS*" "*CMP-GLOBAL-MACROS*"
		"*CMP-MACROS*" "*CMP-SPECIALS*"	"ADD-METHOD"))

(require (make-pathname :name "backquot" :directory '(:relative "compiler")))
(require (make-pathname :name "cmpmacro" :directory '(:relative "compiler")))
(require (make-pathname :name "convert" :directory '(:relative "compiler")))
(require (make-pathname :name "cells" :directory '(:relative "compiler")))
(require (make-pathname :name "simplify" :directory '(:relative "compiler")))
(require (make-pathname :name "lift" :directory '(:relative "compiler")))
(require (make-pathname :name "gencode" :directory '(:relative "compiler")))
(require (make-pathname :name "peephole" :directory '(:relative "compiler")))
(require (make-pathname :name "assemble" :directory '(:relative "compiler")))
(require (make-pathname :name "cmpfront" :directory '(:relative "compiler")))
