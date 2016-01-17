(provide "wrappers")
(defpackage "C-WRAPPERS" (:nicknames "WRAP") (:use "XLISP"))
(in-package "C-WRAPPERS")

(export '(make-wrappers
          c-lines c-constant c-variable c-function c-subr c-pointer c-version))

(defvar *wrapper-functions*)
(defvar *wrapper-fixnum-constants*)
(defvar *wrapper-unsigned-constants*)
(defvar *wrapper-flonum-constants*)
(defvar *wrapper-string-constants*)
(defvar *wrapper-cptr-types*)
(defvar *wrapper-module-version*)
(defvar *c-output*)
(defun write-c-line (fmt &rest args)
  (format *c-output* "~&~?~%" fmt args)
  nil)
(defun make-wrappers (file &key (name (pathname-name file)))
  (unless (equal (pathname-type file) "wrp")
          (error "file ~a does not have a .wrp extension" file))
  (let ((c-file (merge-pathnames (make-pathname :name name :type "c") file))
        (lisp-file (merge-pathnames (make-pathname :name name :type "lsp") file))
        (*package* *package*)
        (*readtable* *readtable*)
        (*wrapper-functions* nil)
        (*wrapper-fixnum-constants* nil)
        (*wrapper-unsigned-constants* nil)
        (*wrapper-flonum-constants* nil)
        (*wrapper-string-constants* nil)
        (*wrapper-cptr-types* nil)
        (*wrapper-module-version* nil)
        (eof (cons nil nil)))
    (with-open-file (in file)
      (with-open-file (*c-output* c-file :direction :output)
        (with-open-file (lisp-out lisp-file :direction :output)
          (write-c-line "/* Generated automatically from ~a by make-wrappers. */" file)
          (write-c-line "#include \"xlshlib.h\"")
          (write-c-line "#include \"xlwrap.h\"")
          (format lisp-out ";; Generated automatically from ~a by make-wrappers.~%" file)
          (loop
           (let ((expr (read in nil eof)))
             (when (eq expr eof) (return))
             (let ((wexpr (wrap-expression expr)))
               (when wexpr
                     (let ((*print-readably* t)
                           (system:*print-symbol-package* t))
                       (format lisp-out "~&~s~%" wexpr))))))
          (let ((path (format nil "(merge-pathnames \"~a.dll\" *load-truename*)" name))
                (vers (if *wrapper-module-version*
                          (let* ((major (first *wrapper-module-version*))
                                 (minor (second *wrapper-module-version*))
                                 (oldmajor (third *wrapper-module-version*))
                                 (oldminor (fourth *wrapper-module-version*))
                                 (vers (+ (* (^ 2 16) major) minor))
                                 (oldvers (+ (* (^ 2 16) oldmajor) oldminor)))
                            (format nil "~d ~d" vers oldvers))
                        nil)))
            (format lisp-out "(shlib::load-shared-library ~a ~s ~@[ ~a~])~%" path name vers))
          (let ((system:*print-symbol-package* t))
            (write-c-line "static FUNDEF ~a_funs[] = {" name)
            (dolist (e (reverse *wrapper-functions*))
              (let ((sym (first e))
                    (fun (second e))
                    (mvals (third e)))
                (unless (symbol-package sym) (error "~s has no package" sym))
                (write-c-line "  { \"~s\", ~:[SUBR~;MVSUBR~], ~a }," sym mvals fun)))
            (write-c-line "  { NULL, 0, NULL}~%};")
            (write-c-line "static FIXCONSTDEF ~a_fixconsts[] = {" name)
            (dolist (e (reverse *wrapper-fixnum-constants*))
              (let ((sym (car e))
                    (val (cdr e)))
                (unless (symbol-package sym) (error "~s has no package" sym))
                (write-c-line "  { \"~s\", ~a }," sym val)))
            (write-c-line "  { NULL, 0}~%};")
            (write-c-line "static FLOCONSTDEF ~a_floconsts[] = {" name)
            (dolist (e (reverse *wrapper-flonum-constants*))
              (let ((sym (car e))
                    (val (cdr e)))
                (unless (symbol-package sym) (error "~s has no package" sym))
                (write-c-line "  { \"~s\", ~a }," sym val)))
            (write-c-line "  { NULL, 0.0}~%};")
            (write-c-line "static STRCONSTDEF ~a_strconsts[] = {" name)
            (dolist (e (reverse *wrapper-string-constants*))
              (let ((sym (car e))
                    (val (cdr e)))
                (unless (symbol-package sym) (error "~s has no package" sym))
                (write-c-line "  { \"~s\", ~a }," sym val)))
            (write-c-line "  { NULL, NULL}~%};")
            (write-c-line "static ULONGCONSTDEF ~a_ulongconsts[] = {" name)
            (dolist (e (reverse *wrapper-unsigned-constants*))
              (let ((sym (car e))
                    (val (cdr e)))
                (unless (symbol-package sym) (error "~s has no package" sym))
                (write-c-line "  { \"~s\", ~a }," sym val)))
            (write-c-line "  { NULL, 0}~%};")
            (let ((vers (if *wrapper-module-version* *wrapper-module-version* '(0 1 0 1))))
              (write-c-line "static xlshlib_modinfo_t ~a_info = {~%~
                            ~2tXLSHLIB_VERSION_INFO(~{~d,~d,~d,~d~}),~%~
                            ~2t~a_funs,~%~
                            ~2t~a_fixconsts,~%~
                            ~2t~a_floconsts,~%~
                            ~2t~a_strconsts,~%~
                            ~2t~a_ulongconsts~%};"
                            name vers name name name name name))
            (write-c-line "xlshlib_modinfo_t *~a__init(void) { return &~a_info; }"
                          name name)))))))
(defun wrap-expression (expr)
  (case (first expr)
        (macrolet (error "top level MACROLET not supported in wrappers"))
        (eval-when
         (let ((sits (second expr)))
           (when (or (member 'compile sits) (member :compile-toplevel sits))
                 (dolist (e (rest (rest expr)))
                   (eval e))))
         expr)
        ((defun defstruct do do* dolist dotimes) expr)
        (progn `(progn ,@(mapcar #'wrap-expression (rest expr))))
        ((in-package defpackage defmacro) (eval expr) expr)
        (t (multiple-value-bind (ee again) (macroexpand expr)
             (if again
                 (wrap-expression ee)
               ee)))))
(defun c-type (type)
  (case type
        (:void "void")
        (:integer "long")
        (:unsigned "unsigned long")
        (:flonum "double")
        (:string "char *")
        (:lval "LVAL")
        (t (cond
            ((stringp type) type)
            ((signed-type-p type) (format nil "signed ~a" (second type)))
            ((unsigned-type-p type) (format nil "unsigned ~a" (second type)))
            ((pointer-type-p type) (format nil "~a *" (c-type (second type))))
            ((struct-type-p type) (format nil "struct ~a" (second type)))
            ((union-type-p type) (format nil "union ~a" (second type)))
            (t (error "type ~s is unknown" type))))))
(defun mangled-type (type)
  (case type
        (:void "void")
        (:integer "long")
        (:unsigned "unsigned_long")
        (:flonum "double")
        (:string "c_string")
        (:lval "LVAL")
        (t (cond
            ((stringp type) type)
            ((signed-type-p type) (format nil "signed_~a" (second type)))
            ((unsigned-type-p type) (format nil "unsigned_~a" (second type)))
            ((pointer-type-p type) (format nil "~a_P" (mangled-type (second type))))
            ((struct-type-p type) (format nil "S_~a" (second type)))
            ((union-type-p type) (format nil "U_~a" (second type)))
            (t (error "type ~s is unknown" type))))))
(defun pointer-type-p (type)
  (and (consp type) (eq (first type) :cptr)))
(defun signed-type-p (type)
  (and (consp type) (eq (first type) :signed)))
(defun unsigned-type-p (type)
  (and (consp type) (eq (first type) :unsigned)))
(defun struct-type-p (type)
  (and (consp type) (eq (first type) :struct)))
(defun union-type-p (type)
  (and (consp type) (eq (first type) :union)))
(defun register-pointer-type (type)
  (unless *wrapper-cptr-types*
          (push "void" *wrapper-cptr-types*)
          (write-c-line "DECLARE_CPTR_TYPE(~a)" "void"))
  (unless (member type *wrapper-cptr-types* :test #'equal)
          (push type *wrapper-cptr-types*)
          (write-c-line "DECLARE_CPTR_TYPE(~a)" (mangled-type type))))
(defmacro c-lines (&rest lines)
  (dolist (ln lines)
    (write-c-line ln)))
(defmacro c-constant (name cname type)
  (case type
        (:integer (push (cons name cname) *wrapper-fixnum-constants*))
        (:unsigned (push (cons name cname) *wrapper-unsigned-constants*))
        (:flonum (push (cons name cname) *wrapper-flonum-constants*))
        (:string (push (cons name cname) *wrapper-string-constants*))
        (t (error "can't handle constants of type ~s" type)))
  nil)
(defmacro c-variable (name type &rest clauses)
  (when (pointer-type-p type)
        (register-pointer-type (second type)))
  (dolist (c clauses)
    (case (first c)
          (:get (write-c-variable-get name type (second c)))
          (:set (write-c-variable-set name type (second c))))))
(defconstant c-variable-get-fmt "~
static LVAL ~a(void) {
  xllastarg();
  return ~@?;
}")
(defun write-c-variable-get (name type fun)
  (let ((cfun (c-function-name "get_~a_var" name)))
    (write-c-line c-variable-get-fmt cfun (c-value-fmt type) name nil)
    (register-subr fun cfun)))
(defun c-function-name (fmt &rest args)
  (format nil "xlw_~?" fmt args))
(defun register-subr (lisp-name c-name &optional mvals)
  (push (list lisp-name c-name mvals) *wrapper-functions*)
  nil)
(defun c-value-fmt (type)
  (case type
        (:void "NIL")
        (:integer "long2lisp(~a)")
        (:unsigned "ulong2lisp(~a)")
        (:flonum "cvflonum(~a)")
        (:string "cvstrornil(~a)")
        (:lval "~a")
        (t (if (pointer-type-p type)
               (format nil "cvcptr(CPTR_TYPE(~a),~~a,~~a)"
                       (mangled-type (second type)))
             (error "can't handle ~a value type" type)))))
(defconstant c-variable-set-fmt "~
static LVAL ~a(void) {
  LVAL xlw__val = xlgetarg();
  xllastarg();
  ~a = ~@?;
  return xlw__val;
}")
(defun write-c-variable-set (name type fun)
  (let ((cfun (c-function-name "set_~a_var" name))
        (afmt (c-argument-fmt type)))
    (write-c-line c-variable-set-fmt cfun name afmt "xlw__val")
    (register-subr fun cfun)))
(defun c-argument-fmt (type)
  (case type
        (:integer "lisp2long(~a)")
        (:unsigned "lisp2ulong(~a)")
        (:flonum "makefloat(~a)")
        (:string "getstring(~a)")
        (:lval "~a")
        (t (error "can't handle ~a argument type" type))))
(defconstant c-function-fmt "~
static LVAL ~a(void)
{
~:{~&  ~a ~a = ~a;~}
~@[~&  ~a xlw__v;~]~&  xllastarg();
  ~@[~*xlw__v = ~]~a(~:[~2*~;~a~{,~a~}~]);
  return ~@?;
}")
(defmacro c-function (name cname args value)
  (dolist (a args)
    (when (pointer-type-p a)
          (register-pointer-type (second a))))
  (when (pointer-type-p value)
        (register-pointer-type (second value)))
  (let* ((fun (c-function-name "_~a_wrap" cname))
         (ainfo (c-function-arginfo args))
         (anames (mapcar #'second ainfo))
         (vt (if (eq value :void) nil (c-type value))))
    (write-c-line c-function-fmt
                  fun
                  ainfo
                  vt
                  vt cname anames (first anames) (rest anames)
                  (c-value-fmt value) "xlw__v" "NIL")
     (register-subr name fun)))
(defun c-function-arginfo (args)
  (let ((val nil)
        (count 0))
    (dolist (a args (nreverse val))
      (incf count)
      (let ((ct (c-type a))
            (v (format nil "xlw__x~d" count))
            (arg (if (pointer-type-p a)
                     (format nil "getcpaddr(xlgacptr(CPTR_TYPE(~a),~a))"
                             (mangled-type (second a))
                             (if (third a) "TRUE" "FALSE"))
                   (format nil (c-argument-fmt a) "xlgetarg()"))))
        (push (list ct v arg) val)))))
(defmacro c-subr (fun cfun &optional mvals)
  (register-subr fun cfun mvals))
(defmacro c-pointer (type &rest clauses)
  (register-pointer-type type)
  (dolist (c clauses)
    (case (first c)
          (:make (write-c-pointer-make type (second c)))
          (:cast (write-c-pointer-cast type (second c)))
          (:offset (write-c-pointer-offset type (second c)))
          (:get (write-c-pointer-get type (second c) (third c) (fourth c)))
          (:set (write-c-pointer-set type (second c) (third c) (fourth c))))))
(defconstant c-pointer-make-fmt "~
static LVAL ~a(void)
{
  return xlw_make_cptr(CPTR_TYPE(~a), sizeof(~a));
}")
(defun write-c-pointer-make (type fun)
  (let* ((mt (mangled-type type))
         (ct (c-type type))
         (cfun (c-function-name "make_~a_cptr" mt)))
    (if (equal type "void")
        (write-c-line c-pointer-make-fmt cfun "void" "char")
      (write-c-line c-pointer-make-fmt cfun mt ct))
    (register-subr fun cfun)))
(defconstant c-pointer-cast-fmt "~
static LVAL ~a()
{
  return xlw_cast_cptr(CPTR_TYPE(~a));
}")
(defun write-c-pointer-cast (type fun)
  (let* ((mt (mangled-type type))
         (cfun (c-function-name "cast_~a_cptr" mt)))
    (write-c-line c-pointer-cast-fmt cfun mt)
    (register-subr fun cfun)))
(defconstant c-pointer-offset-fmt "~
static LVAL ~a(void)
{
  return xlw_offset_cptr(CPTR_TYPE(~a), sizeof(~a));
}")
(defun write-c-pointer-offset (type fun)
  (let* ((mt (mangled-type type))
         (ct (c-type type))
         (cfun (c-function-name "offset_~a_cptr" mt)))
    (if (equal type "void")
        (write-c-line c-pointer-offset-fmt cfun "void" "char")
      (write-c-line c-pointer-offset-fmt cfun mt ct))
    (register-subr fun cfun)))
(defconstant c-pointer-get-fmt "~
static LVAL ~a(void)
{
  LVAL p = xlgacptr(CPTR_TYPE(~a), FALSE);
  ~a *x = getcpaddr(p);
  FIXTYPE off = moreargs() ? getfixnum(xlgafixnum()) : 0;
  xllastarg();
  return ~@?;
}")
(defun write-c-pointer-get (type fun field vtype)
  (when (pointer-type-p vtype)
        (register-pointer-type (second vtype)))
  (let* ((ct (c-type type))
         (mt (mangled-type type))
         (cfun (c-function-name "get_~a~@[_~a~]" mt field))
         (val (format nil "x[off]~@[.~a~]" field))
         (cvt (c-value-fmt vtype)))
    (write-c-line c-pointer-get-fmt cfun mt ct cvt val "p")
    (register-subr fun cfun)))
(defconstant c-pointer-set-fmt "~
static LVAL ~a(void)
{
  ~a *x = getcpaddr(xlgacptr(CPTR_TYPE(~a), FALSE));
  LVAL val = xlgetarg();
  FIXTYPE off = moreargs() ? getfixnum(xlgafixnum()) : 0;
  xllastarg();
  x[off]~@[.~a~] = ~@?;
  return val;
}")
(defun write-c-pointer-set (type fun field vtype)
  (when (pointer-type-p vtype)
        (register-pointer-type (second vtype)))
  (let* ((mt (mangled-type type))
         (ct (c-type type))
         (cfun (c-function-name "set_~a~@[_~a~]" mt field))
         (cvt (c-argument-fmt vtype)))
    (write-c-line c-pointer-set-fmt cfun ct mt field cvt "val")
    (register-subr fun cfun)))
(defmacro c-version (&optional (major 0) (minor 0) 
                               (oldmajor major) (oldminor minor))
  (setf *wrapper-module-version* (list major minor oldmajor oldminor))
  nil)
(defconstant c-callback-fmt "~
static ~a ~a(~a x1, ~a x2)
{
  LVAL xlw_x1, xlw_x2, xlw_v;
  static LVAL fsym = NULL;
  if (fsym == NULL)
    fsym = xlenter(\"~a\");
  xlstkcheck(2);
  xlsave(2);
  xlw_x1 = ~@?;
  xlw_x1 = ~@?;
  xlw_v = xlappn(xlgetfunction(fsym), 2, xlw_x1, xlw_x2);
  xlpopn(2);
  return ...;
}")
