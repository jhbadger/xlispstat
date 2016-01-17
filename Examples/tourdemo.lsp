#+macintosh(require "tour" ":Examples:tour")
#+macintosh(require ":Data:iris")
#+unix(require "tour" "Examples/tour")
#+unix(load-data "iris")
#+msdos(require "tour" "Examples\\tour")
#+msdos(load-data "iris")

(tour-plot 
 (mapcar  #'(lambda (x) (select x (* 2 (iseq 0 74)))) iris) 
 :variable-labels '("X" "Y" "Z" "W"))
