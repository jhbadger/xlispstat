; Many people have had trouble creating an initial workspace where the tools
; package is accessable. This sample LSP file will create a workspace which
; will be loaded by default.
; To build, delete any existing xlisp.wks file, run xlisp, and then load
; this file. You may want to customize for the actual tools you want.
(expand 5)		; Or whatever you want -- object here is to make
			; an enlarged image to reduce garbage collections.
(load "common")		; Common Lisp extensions
(load "classes")	; Classes
(load "stepper")	; Stepper tool, STEP
(load "pp")		; Pretty printer tool
(load "common2")	; more Common Lisp extensions
(load "inspect")        ; Inspector (has INSPECT and DESCRIBE)
;(load "repair")        ; Old version of Inspect
(use-package :tools)	; makes package :tools accessable
(load "document")	; Glossary (glos.txt must be in current directory)
                        ;  and DOCUMENTATION function
; Instead of "document", you might want "glos"
;(load "glos")          ; Glossary, without documentation function
(save "xlisp")		; save image

