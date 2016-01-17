(defpackage "SOCKETS" (:use "COMMON-LISP"))
(in-package "SOCKETS")
(shlib::load-shared-library (merge-pathnames "xlsock.dll" *load-truename*))

(export '(with-client-socket socket-read-line socket-write-line
         socket-force-output socket-write-string
         socket-read-byte
         with-server-socket-loop))

(defstruct (socket (:constructor (make-socket (fd))))
  fd (inbuf (make-string 1024)) (instart 0) (inend 0) ineof)

(defun close-socket (sock)
  (let ((fd (socket-fd sock)))
    (when fd (sock-close fd) (setf (socket-fd sock) nil))
    nil))

(defmacro with-client-socket ((sock host port) &rest body)
  (let ((fdsym (gensym)))
    `(let ((,fdsym (sock-connect ,host ,port)))
       (unless ,fdsym (error "Unable to establish an Internet connection"))
       (let ((,sock (make-socket ,fdsym)))
         (unwind-protect
             (progn ,@body)
           (close-socket ,sock))))))

(defun run-server-loop (port fun fork)
  (let ((listenfd (sock-open port)))
    (unless listenfd (error "Unable to establish a port connection"))
    (unwind-protect
        (loop
         (let ((communfd (sock-listen listenfd)))
           (unless communfd (error "Failure to listen on server"))
           (let ((sock (make-socket communfd)))
             (if fork
                 (let ((pid (fork)))
                   (unless pid "error failure to fork")
                   (case pid
                         (0 (handle-connection sock fun) (exit))
                         (otherwise (close-socket sock))))
               (handle-connection sock fun)))))
      (sock-close listenfd))))

(defun handle-connection (sock fun)
  (unwind-protect
      (funcall fun sock)
    (close-socket sock)))

(defmacro with-server-socket-loop ((sock port &key fork) &rest body)
  `(run-server-loop ,port #'(lambda (,sock) ,@body) ,fork))

(defun socket-write-string (str sock &optional start end)
  (unless start (setf start 0))
  (unless end (setf end (length str)))
  (let ((fd (socket-fd sock)))
    (loop
     (when (<= end start) (return str))
     (let ((count (base-sock-write fd str start end)))
       (unless count (error "socket write failed after ~d bytes" start))
       (incf start count)))))

(defun socket-write-line (str sock &optional start end)
  (socket-write-string str sock start end)
  (socket-write-string "\r\n" sock start end))

(defun socket-force-output (sock) nil)

(defun base-sock-read-char (sock &optional eoferrp eofval recp)
  (if (socket-ineof sock)
      (if eoferrp
          (error "end of file")
        eofval)
    (let ((start (socket-instart sock))
          (end (socket-inend sock)))
      (if (<= end start)
          (let ((count (base-sock-read (socket-fd sock) (socket-inbuf sock))))
            (unless count (error "socket read error"))
            (if (= count 0)
                (setf (socket-ineof sock) t)
              (setf (socket-instart sock) 0
                    (socket-inend sock) count))
            (base-sock-read-char sock eoferrp eofval recp))
        (let ((ch (char (socket-inbuf sock) start)))
          (setf (socket-instart sock) (+ start 1))
          ch)))))

(defun base-sock-peek-char (type sock &optional eoferrp eofval recp)
  (let ((ch (base-sock-read-char sock eoferrp eofval recp)))
    (decf (socket-instart sock))
    ch))

(defun socket-read-char (sock &optional eoferrp eofval recp)
  (let ((ch (base-sock-read-char sock eoferrp eofval recp)))
    (if (eql ch #\return)
        (let ((next (base-sock-peek-char nil sock nil nil)))
          (cond
           ((eql next #\newline) (base-sock-read-char sock) #\newline)
           (t #\return)))
      ch)))

(defun socket-read-byte (sock &optional eoferrp eofval)
  (let ((ch (base-sock-read-char sock eoferrp nil)))
    (if ch
        (char-int ch)
      eofval)))

(defun socket-read-line (sock &optional eoferrp eofval recp)
  (let ((ch (socket-read-char sock eoferrp nil recp))
        (nlmissing nil))
    (if ch
        (values
         (with-output-to-string (s)
           (loop
            (when (null ch) (setf nlmissing t) (return))
            (when (eql ch #\newline) (return))
            (write-char ch s)
            (setf ch (socket-read-char sock nil nil))))
         nlmissing)
      (values eofval t))))
