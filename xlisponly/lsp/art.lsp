; This is an example using the object-oriented programming support in
; XLISP.  The example involves defining a class of objects representing
; dictionaries.  Each instance of this class will be a dictionary in
; which names and values can be stored.  There will also be a facility
; for finding the values associated with names after they have been
; stored.

; Create the 'Dictionary' class and establish its instance variable list.
; The variable 'entries' will point to an association list representing the
; entries in the dictionary instance.

(setq Dictionary (send Class :new '(entries)))

; Setup the method for the ':isnew' initialization message.
; This message will be send whenever a new instance of the 'Dictionary'
; class is created.  Its purpose is to allow the new instance to be
; initialized before any other messages are sent to it.  It sets the value
; of 'entries' to nil to indicate that the dictionary is empty.

(send Dictionary :answer :isnew '()
	    '((setq entries nil)
	      self))

; Define the message ':add' to make a new entry in the dictionary.  This
; message takes two arguments.  The argument 'name' specifies the name
; of the new entry; the argument 'value' specifies the value to be
; associated with that name.

(send Dictionary :answer :add '(name value)
	    '((setq entries
	            (cons (cons name value) entries))
	      value))

; Create an instance of the 'Dictionary' class.  This instance is an empty
; dictionary to which words may be added.

(setq d (send Dictionary :new))

; Add some entries to the new dictionary.

(send d :add 'mozart 'composer)
(send d :add 'winston 'computer-scientist)

; Define a message to find entries in a dictionary.  This message takes
; one argument 'name' which specifies the name of the entry for which to
; search.  It returns the value associated with the entry if one is
; present in the dictionary.  Otherwise, it returns nil.

(send Dictionary :answer :find '(name &aux entry)
	    '((cond ((setq entry (assoc name entries))
	      (cdr entry))
	     (t
	      nil))))

; Try to find some entries in the dictionary we created.

(send d :find 'mozart)
(send d :find 'winston)
(send d :find 'bozo)

; The names 'mozart' and 'winston' are found in the dictionary so their
; values 'composer' and 'computer-scientist' are returned.  The name 'bozo'
; is not found so nil is returned in this case.
 