(use-package "SOCKETS")

(defun server (port &key (fork t))
  (with-server-socket-loop (sock port :fork fork)
    (format *error-output* "Connection has been made to ~a~%" "????")
    (loop
     (let ((byte (socket-read-byte sock nil nil)))
         (if byte
             (write-char (int-char byte))
           (return))))
      (format *error-output* "socket closed by client~%")))

(defun client (host port)
  (with-client-socket (sock port host)
    (format *error-output* "Connection has been made to ~a:~d~%" host port)
    (let ((nl (string #\newline)))
      (loop
       (let ((line (read-line *standard-input* nil nil)))
         (unless line (return))
         (socket-write-string line sock)
         (socket-write-string nl sock)
         (socket-force-output sock))))))

(defun finger (user &optional (host "localhost") (port 79))
  (with-client-socket (sock port host)
    (socket-write-line user sock)
    (socket-force-output sock)
    (with-output-to-string (s)
      (loop
       (multiple-value-bind (line nlmissing)
                            (socket-read-line sock nil nil)
          (unless line (return))
          (write-string line s)
          (unless nlmissing (terpri s)))))))

(defun echo (&optional (host "localhost") (port 7))
  (with-client-socket (sock port host)
    (format *error-output* "Connection has been made to ~a:~d~%" host port)
    (loop
     (let ((line (read-line *standard-input* nil nil)))
       (unless line (return))
       (socket-write-line line sock)
       (socket-force-output sock)
       (write-line (socket-read-line sock))))))

(defun daytime (&optional (host "localhost") (port 13))
  (with-client-socket (sock port host)
    (socket-read-line sock)))
