(provide "regexp")

(defpackage "REGULAR-EXPRESSIONS"
  (:use "COMMON-LISP")
  (:nicknames "REGEXP"))

(in-package "REGEXP")

(export '(REG_EXTENDED REG_NEWLINE REG_NOSUB REG_ICASE REG_NOTBOL REG_NOTEOL
          regcomp regexec
          regexp regsub url-decode))

(system:define-autoload-module "regexp"
  (variable REG_EXTENDED REG_NEWLINE REG_NOSUB REG_ICASE REG_NOTBOL REG_NOTEOL)
  (function regcomp regexec
            regexp regsub url-decode))
