(defpackage "SOCKETS" (:use "COMMON-LISP"))
(in-package "SOCKETS")

(export '(with-client-socket socket-read-line socket-write-line
         socket-force-output socket-write-string
         with-server-socket-loop))

(system:define-autoload-module "sock"
  (function with-client-socket socket-read-line socket-write-line
	    socket-force-output socket-write-string
	    socket-read-byte
	    with-server-socket-loop))
