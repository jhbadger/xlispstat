(provide "wrappers")
(defpackage "C-WRAPPERS" (:nicknames "WRAP") (:use "XLISP"))
(in-package "C-WRAPPERS")

(export '(make-wrappers
          c-lines c-constant c-variable c-function c-subr c-pointer c-version))

(system:define-autoload-module "wrap"
  (function make-wrappers
            c-lines c-constant c-variable c-function c-subr c-pointer c-version))

(provide "wrapptrs")
(defpackage "POINTER-WRAPPERS" (:nicknames "WRAPPTRS") (:use "XLISP"))
(in-package "WRAPPTRS")

(export '(make-c-void cast-c-void
          make-c-void-p cast-c-void-p offset-c-void-p get-c-void-p
          make-c-char cast-c-char offset-c-char get-c-char set-c-char
          make-c-schar cast-c-schar offset-c-schar get-c-schar set-c-schar
          make-c-uchar cast-c-uchar offset-c-uchar get-c-uchar set-c-uchar
          make-c-short cast-c-short offset-c-short get-c-short set-c-short
          make-c-ushort cast-c-ushort offset-c-ushort get-c-ushort set-c-ushort
          make-c-int cast-c-int offset-c-int get-c-int set-c-int
          make-c-uint cast-c-uint offset-c-uint get-c-uint set-c-uint
          make-c-long cast-c-long offset-c-long get-c-long set-c-long
          make-c-ulong cast-c-ulong offset-c-ulong get-c-ulong set-c-ulong
          make-c-float cast-c-float offset-c-float get-c-float set-c-float
          make-c-double cast-c-double offset-c-double get-c-double set-c-double
          make-c-string cast-c-string offset-c-string get-c-string set-c-string))

(system:define-autoload-module "wrapptrs"
  (function make-c-void cast-c-void
            make-c-void-p cast-c-void-p offset-c-void-p get-c-void-p
            make-c-char cast-c-char offset-c-char get-c-char set-c-char
            make-c-schar cast-c-schar offset-c-schar get-c-schar set-c-schar
            make-c-uchar cast-c-uchar offset-c-uchar get-c-uchar set-c-uchar
            make-c-short cast-c-short offset-c-short get-c-short set-c-short
            make-c-ushort cast-c-ushort offset-c-ushort get-c-ushort set-c-ushort
            make-c-int cast-c-int offset-c-int get-c-int set-c-int
            make-c-uint cast-c-uint offset-c-uint get-c-uint set-c-uint
            make-c-long cast-c-long offset-c-long get-c-long set-c-long
            make-c-ulong cast-c-ulong offset-c-ulong get-c-ulong set-c-ulong
            make-c-float cast-c-float offset-c-float get-c-float set-c-float
            make-c-double cast-c-double offset-c-double get-c-double set-c-double
            make-c-string cast-c-string offset-c-string get-c-string set-c-string))
