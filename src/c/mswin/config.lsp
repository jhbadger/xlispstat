(defpackage setup (:use xlisp))

(in-package setup)

(defvar *progman-available*
        (let ((conv (dde-connect "progman")))
          (if conv (dde-disconnect conv))
          conv))

(defproto preference-item-proto 
  '(section name default) () edit-text-item-proto)

(defmeth preference-item-proto :isnew (section name default &rest args)
  (setf (slot-value 'section) section)
  (setf (slot-value 'name) name)
  (setf (slot-value 'default) default)
  (apply #'call-next-method default args))

(defmeth preference-item-proto :configure ()
  (msw-write-profile-string (slot-value 'section)
                            (slot-value 'name)
                            (send self :text)
                            #+win32 "wxls32.ini"
                            #-win32 "wxls.ini"))

(defproto directory-item-proto () () preference-item-proto)

(defmeth directory-item-proto :isnew ()
  (call-next-method "Xlisp" 
                    "Libdir"
                    (get-working-directory)
                    :text-length 25))


(defproto font-name-item-proto () () preference-item-proto)

(defmeth font-name-item-proto :isnew (section &optional 
                                              (font "Courier New")
                                              (size 16))
  (call-next-method section "Font" font :text-length size))

(defproto font-size-item-proto () () preference-item-proto)

(defmeth font-size-item-proto :isnew (section &optional (size 12))
  (call-next-method section "FontSize" (format nil "~d" size)))

(defmeth font-size-item-proto :configure ()
  (let ((n (read-from-string (send self :text) nil)))
    (unless (integerp n)
            (send self :text (slot-value 'default)))
    (call-next-method)))

(defproto progman-item-proto () () edit-text-item-proto)

(defmeth progman-item-proto :exec (&rest args)
  (let ((conv (dde-connect "progman")))
   (when conv 
         (unwind-protect
          (dde-client-transaction conv :data (apply #'format nil args))
          (dde-disconnect conv)))))

(defproto progman-group-item-proto () () progman-item-proto)

(defun progman-group-exists (group)
  (let ((conv (dde-connect "progman")))
   (when conv 
         (unwind-protect
          (dde-client-transaction conv :type :request :item group)
          (dde-disconnect conv)))))


(defmeth progman-group-item-proto :configure ()
  (let ((group (send self :text)))
    (when (progman-group-exists group)
          (let ((delete (ok-or-cancel-dialog
                         (format nil "Delete existing ~a group?"
                                 group))))
            (if delete
                (send self :exec "[DeleteGroup(~a)]" group)
                (throw 'cancel nil)))))
  (send self :exec "[CreateGroup(~a)]" (send self :text)))

(defproto progman-program-item-proto
 '(application directory icon x y) () progman-item-proto)

(defmeth progman-program-item-proto :isnew (n a d i x y)
  (setf (slot-value 'application) a)
  (setf (slot-value 'directory) d)
  (setf (slot-value 'icon) i)
  (setf (slot-value 'x) x)
  (setf (slot-value 'y) y)
  (call-next-method n))

(defmeth progman-program-item-proto :configure ()
  (let* ((dir (send (slot-value 'directory) :text))
         (app (slot-value 'application))
         (file (format nil "~a\\~a" dir app)))
    (send self :exec "[AddItem(~a,~a,~a,~d,~d,~d,~a)]"
          file
          (send self :text)
          file
          (slot-value 'icon)
          (slot-value 'x)
          (slot-value 'y)
          (send (slot-value 'directory) :text))))

(let* ((list-font-size 12)
       (graph-font-size 12)
       (dir-item (send directory-item-proto :new))
       (list-font-item (send font-name-item-proto :new "Listener"))
       (list-font-size-item (send font-size-item-proto :new "Listener"))
       (graph-font-item (send font-name-item-proto :new "Graphics"))
       (graph-font-size-item (send font-size-item-proto :new "Graphics"))
       (progman-item (send toggle-item-proto :new
                           "Add Group and Items to Program Manager"
                           :value t))
       (group-name-item (send progman-group-item-proto :new
                              (format nil "XLISP-STAT ~d.~d~a"
                                      xls-major-release
                                      xls-minor-release
                                      #+win32 " - Win32"
                                      #-win32 "")))
       (xls-name-item (send progman-program-item-proto :new
                            "XLISP-STAT"
                            #+win32 "WXLS32.EXE"
                            #-win32 "WXLS.EXE"
                            dir-item 2 30 20))
       (lspedit-name-item (send progman-program-item-proto :new
                                "Lsp Edit" "LSPEDIT.EXE"
                                dir-item 0 90 20)))
  (flet ((configure ()
           (catch 'cancel
                  (send dir-item :configure)
                  (send list-font-item :configure)
                  (send list-font-size-item :configure)
                  (send graph-font-item :configure)
                  (send graph-font-size-item :configure)
                  (when (and *progman-available*
                             (send progman-item :value))
                        (send group-name-item :configure)
                        (send xls-name-item :configure)
                        (send lspedit-name-item :configure)))
           (msw-exit))
         (quit () (msw-exit)))
    (let ((pref-items (list (list "Startup Directory:" dir-item)
                            (list
                             (list
                              (list "Listener Font:" list-font-item)
                              (list "Graphics Font:" graph-font-item))
                             (list
                              (list "Size:" list-font-size-item)
                              (list "Size:" graph-font-size-item)))))
          (pm-items (list progman-item
                          (list "Program Group Name:" group-name-item)
                          (list (format nil "Program Item Name for ~a:"
                                        #+win32 "WXLS32.EXE"
                                        #-win32 "WXLS.EXE")
                                xls-name-item)
                          (list "Program Item Name for LSPEDIT.EXE:"
                                lspedit-name-item)))
          (buttons (list (list (send button-item-proto :new
                                     "Configure and Quit"
                                     :action #'configure)
                               (send button-item-proto :new
                                     "Quit"
                                     :action #'quit)))))
    (send dialog-proto :new
          (append pref-items
                  (if *progman-available* pm-items)
                  buttons)    
          :title "XLISP-STAT Setup"
          :location '(0 0)))))

(unless (probe-file "Data\\absorbtion.lsp")
	;; looks like a system that supports long file names -- rename
        ;; some files in Data and Examples.
	(rename-file "Data\\absorbti.lsp" "Data\\absorbtion.lsp")
	(rename-file "Data\\car-pric.lsp" "Data\\car-prices.lsp")
	(rename-file "Data\\metaboli.lsp" "Data\\metabolism.lsp")
	(rename-file "Data\\puromyci.lsp" "Data\\puromycin.lsp")
	(rename-file "Data\\stacklos.lsp" "Data\\stackloss.lsp")
	(rename-file "Examples\\abrasion.lsp" "Examples\\abrasiondemo.lsp")
	(rename-file "Examples\\addhandr.lsp" "Examples\\addhandrotate.lsp")
	(rename-file "Examples\\dataprot.lsp" "Examples\\dataprotos.lsp")
	(rename-file "Examples\\plotcont.lsp" "Examples\\plotcontrols.lsp")
	(rename-file "Examples\\rotatede.lsp" "Examples\\rotatedemo.lsp")
	(when (probe-file "xlsclien.exe")
	  (rename-file "xlsclien.exe" "xlsclient.exe")))
