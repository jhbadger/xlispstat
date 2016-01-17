;;;; XLISP-STAT 2.1 Copyright (c) 1990-1997, by Luke Tierney
;;;; Additions to Xlisp 2.1, Copyright (c) 1989 by David Michael Betz
;;;; You may give out copies of this software; for conditions see the file
;;;; COPYING included with this distribution.

(in-package "USER")
(system:define-autoload-module "nonlin"
  (variable nreg-model-proto)
  (function nreg-model))

(in-package "USER")
(system:define-autoload-module "oneway"
  (variable oneway-model-proto)
  (function oneway-model))

(in-package "XLISP")
(export '(numgrad numhess newtonmax nelmeadmax))
(system:define-autoload-module "maximize"
  (function numgrad numhess newtonmax nelmeadmax))

(in-package "USER")
(system:define-autoload-module "bayes"
  (function bayes-model)
  (variable bayes-model-proto))

(in-package "XLISP")
(export 'step)
(system:define-autoload-module "stepper"
  (function step))

(in-package "XLISP")
(export '(compile compile-file))
(system:define-autoload-module "cmpload"
  (function compile compile-file))

(in-package "USER")
(system:define-autoload-module "glim"
  (variable glim-link-proto identity-link log-link inverse-link sqrt-link
            power-link-proto logit-link probit-link cloglog-link glim-proto
            normalreg-proto poissonreg-proto binomialreg-proto gammareg-proto)
  (function normalreg-model poissonreg-model loglinreg-model binomialreg-model
            logitreg-model probitreg-model gammareg-model indicators
            cross-terms level-names cross-names))
