; Demonstration of classes in XLISP by implementing various Smalltalk
; Collection classes.
; Author: Tom Almy
; Date:   September 1996

; NOTE -- you should probably check out EXAMPLE.LSP, TURTLES.LSP, and
; BLOCKS.LSP first as they are somewhat simpler.

#-:classes (load "classes")	; We'll use these nice macros

; We will put everyting in a package to keep it out of the user name space

#+:packages (unless (find-package "CLASSES")
		    (make-package "CLASSES" :use '("XLISP")))

(in-package "CLASSES")

; List the symbols available on the outside -- in this case the class names.
; The message selectors are all in the keyword package so they don't need
; to be exported.

(export '(Collection Set Bag Dictionary SequenceableCollection
          Array OrderedCollection SortedCollection Interval))
		     


; Our basic Collection class is "abstract" -- it's just defined to
; subclass into useful types of collections. We'll define a single instance
; variable: "data" contains the collection's data, the format to be
; defined by the subclass. Various subclasses will define any additional
; instance variables.

; The actual collections used in applications will be created from subclasses
; of Collection. This demo will implement:

;  Bag -- an unordered collection of objects
;  Set -- like a bag, but no duplicate elements
;  Dictionary -- access elements using symbolic keys
;  SequenceableCollection -- Abstract class which is subclassed into:
 ;  Array -- elements have a sequence. Collection has a fixed size
 ;  OrderedCollection -- same as Array but no fixed size, can add/delete from
 ;         either end.
   ;  SortedCollection -- An Ordered collection with a colating sequence
 ;  Interval -- contains a constant sequence


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE COLLECTION CLASS                                   ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Collection (data))

; The defclass macro defines a new class, which is bound to the symbol
; "Collection". The macro also defines several default methods for instance
; variable access (:data in this case), and instance initialization (:isnew).

; Unlike Smalltalk, XLISP has no class methods. In Smalltalk you create an
; instance of a class by sending a message to the class. In XLISP, the classes
; are members of class Class, and you create an instance by sending the class
; a message with the selector :new. i.e. (send MyClass :new <xxx>) where xxx
; are 0 or more arbitrary expressions. This executes the :new method in class
; Class which will create an object which is a member of the desired class
; (call it newobj) and then does a (send newobj :isnew <xxx>). The :isnew
; method gets the expressions, which it can then use to customize the
; initiation. So, basically, the :isnew method in a class takes the place of
; the class methods of Smalltalk. Not as functional, but it usually does all
; that is needed. The class Array demonstrates how two instance creation
; methods can be supported via the use of keyword arguments.

;;;;;;;;;
; The following group of methods are "private" methods in that they are not
; intended for use in applications, but just to aid in implementation.

; :notImplemented provides a nice error message for messages that aren't
; handled by our class. It's not really necessary to define these!

(defmethod Collection :notImplemented (msg)
	   (error "~s not handled in class ~a"
		  msg
		  (send (send self :class) :pname)))

; Here's the difference:
; >(send x :foo)                                    :notImplemented USED
; error: no method for this message - :foo 

; >(send x :foo)                                :notImplemented NOT USED
; error: :foo not handled in class Bag

; :map is a mapcar like mapping function for the collection.
; This version only works when data instance variable is sequence of
; collection elements. We will have to override the method for subclasses
; that maintain their data differently.

(defmethod Collection :map (fcn) (map 'cons fcn data))

; :addAll will add the elements in the argument collection to this
; collection. We'll extend this definition so it works with sequences as
; well. It won't work with Arrays (which are a fixed size), Intervals
; (which are not alterable), or Dictionaries (which require keys).

(defmethod Collection :addAll (arg)
	   (if (or (listp arg) (arrayp arg) (stringp arg))
	       ; Use map when argument is a sequence
	       (map nil (lambda (x) (send self :add x)) arg)
	       ; Otherwise, send :map to the argument collection
	       (send arg :map (lambda (x) (send self :add x))))
	   self)

; Override default "isnew" to disallow creating abstract collections.
; There is no reason for any program to create an instance of Collection.

(defmethod Collection :isnew (&rest dummy)
	   (error "Don't create collections of class \"Collection\""))

;;;;;;;;;
; Now we will define some "public" methods for Collection. Most will be
; overriden in a subclass. The rest we will provide with a common default
; functionality.


; :prin1 determines how an object is printed. The default is to print
; the objects class and unique ID. We want to do better than that if the
; collection is small enough to easily display, say 5 or fewer elements

(defmethod Collection :prin1 (&optional (stream *standard-output*))
	   (let ((contents (send self :asList)) ; get collection as a list
		 (cls(send(send self :class):pname))) ; and get our class' name
		(cond ((null contents)
		       (format stream "#<An empty ~a>" cls))
		      ((< (length contents) 6)
		       (format stream "#<~a:~{ ~s~}>" cls contents))
		      (t
		       (format stream "#<~a:~5{ ~s~} ...>" cls contents)))))
		       

; :storeon is used to create an expression which, when executed, will create a
; copy of the object. The Default method, part of class Object, won't work
; for classes that override :isnew, and all Collection classes do.

(defmethod Collection :storeon ()
	   (list 'send
		 (list 'send
		       (intern (send (send self :class) :pname))
		       :new)
		 :addAll
		 (list
		  'quote
		  (send self :asList))))

; :at will fetch an element from an "sequenceable collection"
; Not all collections have the concept of sequencing.

(defmethod Collection :at (arg) (send self :notImplemented :at))

; :atput will store an element into a "sequenceable collection".

(defmethod Collection :atPut (arg1 arg2) (send self :notImplemented :atPut))

; :first will fetch the first element of the collection, where appropriate.
; :last does the same thing but for the last element.

(defmethod Collection :first () (send self :notImplemented :first))
(defmethod Collection :last () (send self :notImplemented :last))

; :add will store (one or more copies of) an element into a collection
; :addFirst will add to the start of a collection. These two are not
; implemented for all classes.

(defmethod Collection :add (arg &optional value)
	   (send self :notImplemented :add))

(defmethod Collection :addFirst (arg) 
	   (send self :notImplemented :addFirst))

(defmethod Collection :addLast (arg) 
	   (send self :notImplemented :addLast))

; Delete the specified, first, or last element

(defmethod Collection :remove (arg)
	   (send self :notImplemented :remove))

(defmethod Collection :removeFirst ()
	   (send self :notImplemented :removeFirst))

(defmethod Collection :removeLast ()
	   (send self :notImplemented :removeLast))

; :size -- Get the size of the the Collection. This will work for
; most subclasses.

(defmethod Collection :size () (length data))

; :empty -- Returns T if collection has no elements

(defmethod Collection :empty () (zerop (send self :size)))

; :includes tells us if a object is a member of the collection
; This version only works when data instance variable is sequence of
; collection elements

(defmethod Collection :includes (arg)
	   (if (position arg data) t nil))

; :species returns the class similar to the current class to create new
; objects

(defmethod Collection :species ()
	   (send self :class))


; :do is like :map but returns nothing
; :collect is like :map, but returns a new collection.
; :select returns a collection of elements for which the predicate function
;    returns non-NIL.
; These are generic enough to work for any of the Collection subclasses
; except Array, which requires an argument to :new,
; however in many cases they could be overridden for speed.
; Smalltalk defines these and a number of similar functions.

(defmethod Collection :do (fcn) (send self :map fcn) nil)

(defmethod Collection :collect (fcn)
	   (send (send (send self :species) :new) :addAll (send self :map fcn)))
		
(defmethod Collection :select (fcn)
	   (let ((result
		  (mapcan (lambda (x)
				  (when (funcall fcn x)
					(list x)))
			  (send self :asList))))
		(send (send (send self :species) :new) :addAll result)))


; Our final assortment of Collection methods create copies of the object in
; one of several Collection subclasses or as an LISP list.

; :asList will return the collection as a LISP linked list.

(defmethod Collection :asList () (send self :map #'identity))

; :asBag will return the collection as a Bag

(defmethod Collection :asBag ()
	   (let ((result (send Bag :new)))
		(send result :addAll self)
		result))

; :asSet will return the collection as a Set

(defmethod Collection :asSet ()
	   (let ((result (send Set :new)))
		(send result :addAll self)
		result))

; :asArray will return the collection as an Array

(defmethod Collection :asArray ()
	   (send Array :new :initial (send self :asList)))
		

; :asOrderedCollection will return the collection as an OrderedCollection

(defmethod Collection :asOrderedCollection ()
	   (let ((result (send OrderedCollection :new)))
		(send result :addAll self)
		result))


; :asSortedCollection will return the collection as an OrderedCollection

(defmethod Collection :asSortedCollection (&optional (fcn '<))
	   (let ((result (send SortedCollection :new fcn)))
		(send result :addAll self)
		result))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE SET CLASS                                          ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Our first collection will be "Set".  Initialization doesn't have to do
; anything since instance variables are initialized to NIL.	   
; We will use "eql" as the equality test
;

(defclass Set () nil Collection)

(defmethod Set :isnew ())


; We will need :add. But we will ignore the count.

(defmethod Set :add (arg &optional (count 1))
	   (setq data (adjoin arg data))
	   ; Methods typically return (or "answer" in Smalltalk)
	   ; the object, which is bound to "self", if there is
	   ; nothing more appropriate.
	   self)

; We also need to define :remove

(defmethod Set :remove (arg)
	   (let ((pos (position arg data))) ; Find (first) instance
		(when pos ; Delete found element
		      (if (zerop pos)
			  (setq data (cdr data))
			  (setf (cdr (nthcdr (1- pos) data))
				(nthcdr (1+ pos) data))))
		self))

; All the other methods inherited from Collection will work fine

; At last we can test out some collections!

; > (setq x (send Set :new))               Create a new set
; #<An empty Set>

; Note that if your system says "#<An empty SET>" that means you have
; *readtable-case* set to :upcase. It's nothing to be concerned about, but
; if you want the output to match, start over with *readtable-case* set to
; :invert.

; > (send x :add 3)                        Add the element "3"
; #<Set: 3>               
; > (send x :add 1)                        Add the element "1"
; #<Set: 1 3>
; > (send x :add 3)                        Add another 3 -- it's ignored!
; #<Set: 1 3>        
; > (send x :addAll '(1 2 3 4 5))          Add five elements
; #<Set: 5 4 2 1 3>

; We see the order has changed! This doesn't matter because these collections
; are defined to have no order.

; > (send x :remove '3)                    Remove element "3"
; #<Set: 5 4 2 1>                  
; > (send x :select #'evenp)               Create a set with even elements of x
; #<Set: 2 4>
; > (send x :collect #'1+)                 Create a set with incremented
;                                          elements of x
; #<Set: 2 3 5 6>
; > (let ((cnt 0)) (send x :do (lambda (x) (incf cnt x))) cnt)
; 12                                       Summing all the elements in the set

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE BAG CLASS                                          ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; Our next Collection class will be "Bag" which is an unordered collection
; of objects that we will implement with a hash table. The table value will
; be the number of occurances of the object in the collection.
; It's difficult to calculate the number of elements in a Bag, so we will
; maintain a running total in a new instance variable, size. The defclass
; function will create a :size method for us!
;

; After we've defined this class, we can finally start testing things out.


(defclass Bag (size) nil Collection)

; Because the data in a Bag will be a hash table instead of a list, we
; need to have "isnew" allocate a hash table.
; The entry equality test will be "eql"

(defmethod Bag :isnew nil
	   (setf (send self :data) (make-hash-table)
		 (send self :size) 0))


; We could have done this with "(setf data (make-hash-table) size 0)"
; but this technique is more rigorous.


; The method :add will insert one or more copies of an object in the collection
; We need to adjust the size instance variable when we add objects

(defmethod Bag :add (arg &optional (count 1))
	   (setf (gethash arg data) (+ (gethash arg data 0) count)
		 size (+ size count))
	   self  ; Most methods return Self if there isn't anything else
	   )     ; that is reasonable

; The method :remove will delete an object from the collection
; We need to adjust the size instance variable when we delete objects

(defmethod Bag :remove (arg)
	   (let ((cnt (gethash arg data)))
		(when cnt ; element found
		      (setq size (1- size))
		      (if (= cnt 1)
			  (remhash arg data) ; delete if count would be 0
			  (setf (gethash arg data) (1- cnt))))
		self
		))

; We have to override the definition of :includes since data is stored
; differently in a bag than as a linked list.

(defmethod Bag :includes (arg)
	   (if (gethash arg data) t nil))

; We have to override the definition of :map since data is stored
; differently in a bag than as a linked list.
; Even though :collect is similar, we don't need to redefine it since
; Collection :collect uses :map to do its work.


(defmethod Bag :map (fcn)
	   (if data  ; If in the rare case data isn't set up, we abort
	       (let (result)
		    (maphash (lambda (arg count)
				     (dotimes (i count)
					      (push (funcall fcn arg) result)))
			     data)
		    (nreverse result))
	       nil))


; Now for some Bag examples:


; > (setq y (send Bag :new))                 Create a new bag, y
; #<An empty Bag>
; > (send y :add 3)                          As with set, add 3, 1, 3
; #<Bag: 3>
; > (send y :add 1)
; #<Bag: 3 1>
; > (send y :add 3)
; #<Bag: 3 3 1>                              Now there can be multiple copies!
; > (send y :addAll x)                       Add all the elements of Set x
; #<Bag: 5 4 3 3 2 ...>                      Elipsis means too many to display
; > (send y :asList)                         Use :asList to see entire contents
; (5 4 3 3 2 1 1)
; > (send y :remove 4)
; #<Bag: 5 3 3 2 1 ...>                      Remove still works
; > (send y :select #'oddp)                  Try :select :collect and :do
; #<Bag: 5 3 3 1 1>
; > (send (send y :collect #'1+) :asList)
; (6 4 4 3 2 2)
; > (let ((cnt 0)) (send y :do (lambda (x) (incf cnt x))) cnt)
; 15
; > (send y :asSet)                          Converting a Bag to a Set
; #<Set: 1 2 3 5>                            will delete duplicates


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE DICTIONARY CLASS                                   ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The Dictionary class will be implemented using a hash table like a Bag.
; The hash table will use #'equal for comparisons, allowing string keys
;

(defclass Dictionary () nil Collection)

(defmethod Dictionary :isnew nil
	   (setf (send self :data) (make-hash-table :test #'equal)))

; Getting the size of a Dictionary is slightly different than the default
	   
(defmethod Dictionary :size () (hash-table-count data))

; We need to define the :at and :atPut methods. :at will be extended
; to allow a keyword argument "ifAbsent" to supply the return value.
; It's a closure, just like in Smalltalk

(setq gened (gensym))  ; We need a unique symbol

(defmethod Dictionary :at (key &key ifAbsent)
	   (let ((value (gethash key data '#.gened)))
		(if (eq value '#.gened)
		    (if ifAbsent
			(funcall ifAbsent)
			nil)
		    value)))


(defmethod Dictionary :atPut (key value)
	   (setf (gethash key data) value)
	   self)


; :addAll needs to be redefined, and requires a list of key-value pairs.
; This method makes :storeon much simpler.

(defmethod Dictionary :addAll (arg)
	   (if (or (listp arg) (arrayp arg) (stringp arg))
	       ; Use map when argument is a sequence
	       (map nil
		    (lambda (x) (send self :atPut (first x) (second x)))
		    arg)
	       ; Otherwise, send :map to the argument collection
	       (send arg
		     :map
		     (lambda (x)
			     (send self :atPut (first x) (second x)))))
	   self)



; :remove won't work for a Dictionary, since we want to remove key/value
; associations. Thus we have :removeKey, with an optional ifAbsent.

(defmethod Dictionary :removeKey (key &key ifAbsent)
	   (if (eq (gethash key data '#.gened) '#.gened)
	       (progn
		(remhash key data)
		(setq count (1- count)))
	       (when ifAbsent (funcall ifabsent)))
	   self)

(unintern gened) ; We don't need this symbol anymore

; :keys returns a set of the keys

(defmethod Dictionary :keys ()
	   (let (list)
		(maphash (lambda (key value) (setq list (cons key list)))
			 data)
		(send (send Set :new) :addAll list)))

; :values returns a bag of the values

(defmethod Dictionary :values ()
	   (let (list)
		(maphash (lambda (key value) (setq list (cons value list)))
			 data)
	   (send (send Bag :new) :addAll list)))

; :map is defined to work over the values

(defmethod Dictionary :map (fcn)
	   (let (list)
		(maphash (lambda (key value)
				 (setq list (cons (funcall fcn value) list)))
			  data)
		list))

; We have to override the definition of :includes since data is stored
; differently in a Dictionary than as a linked list.

(defmethod Dictionary :includes (arg)
	   (if (position arg (send self :asList)) t nil))

; :collect, :select aren't appropriate

(defmethod Dictionary :collect (arg) 
	   (send self :notImplemented :collect))

(defmethod Dictionary :select (arg) 
	   (send self :notImplemented :select))

; :prin1 needs to be overridden to show both keys and data

(defmethod Dictionary :prin1 (&optional (stream *standard-output*))
	   (let (contents ; get collection as a list
			  ; and get our class' name
			  ; (it might not be "Dictionary")
	         (cls (send (send self :class) :pname))) 
		(maphash (lambda (x y)
				 (setq contents (cons (list x y) contents)))
			 data)
		(cond ((null contents)
		       (format stream
			       "#<An empty ~a>" cls))
		      ((< (length contents) 6)
		       (format stream
			       "#<~a:~{ ~s~}>" cls contents))
		      (t
		       (format stream
			       "#<~a:~5{ ~s~} ...>" cls contents)))))


; A different :storeon is needed as well

(defmethod Dictionary :storeon ()
	   (let (contents) ; get collection as a list
		(maphash (lambda (x y)
				 (setq contents (cons (list x y) contents)))
			 data)
		(list 'send
		      (list 'send 'Dictionary :new)
		 :addAll
		 (list
		  'quote
		  contents))))

; Class Dictionary examples

; > (setq z (send Dictionary :new))              Create a new dictionary
; #<An empty Dictionary>
; > (send z :addAll '((a 1) (b 2) (c 3) (d 4)))  Quickly add 4 entries
; #<Dictionary: (a 1) (b 2) (c 3) (d 4)>
; > (send z :at 'b)                              Given a key, returns value
; 2
; > (send z :at 'e :ifAbsent (lambda () "Key Not Found")) Check ":ifAbsent"
; "Key Not Found"
; > (send z :atPut 'b 7)                         :atPut will change value
; #<Dictionary: (a 1) (b 7) (c 3) (d 4)>
; > (send z :atPut 'e 100)                       :atPut will create new entries
; #<Dictionary: (a 1) (b 7) (c 3) (d 4) (e 100)>
; > (send z :asBag)                              Converting to Bag just gives
; #<Bag: 7 100 4 3 1>                            values



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE SEQUENCEABLECOLLECTION CLASS                       ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The class SequenceableCollection is, like Collection, an abstract class.
; This is a good thing since who would want to type "SequenceableCollection"
; very often?
;

(defclass SequenceableCollection () nil Collection)

; Some methods can be defined that will work for all subclasses of
; SequenceableCollection. The minimum index value is 0.

(defmethod SequenceableCollection :at (arg) (elt data arg))

(defmethod SequenceableCollection :atPut (arg value)
	   (setf (elt data arg) value)
	   self)

(defmethod SequenceableCollection :first () (self :at 0))

(defmethod SequenceableCollection :last () (self :at (1- (send self :size))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE ARRAY CLASS                                        ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The Array class -- implemented using an array.
; Because it has a fixed size, we have to allocate space for it when
; we create it. We will allow for initialization, since :addAll won't work.
; Either : (send Array :new :size 10) for example, to create an array of 10
; entries or (send Array :new :initial (1 2 3 4 5)) for an initialized array.


(defclass Array () nil SequenceableCollection)

(defmethod Array :isnew (&key size initial)
	   ; Size must be specified when creating array
	   (if size
	       (setf (send self :data) (make-array size))
	       (setf (send self :data)
		     (make-array (length initial) :initial-contents initial))))

; We have to override :collect because (send Array :new) won't work.
; But we can optimize while we are at it.

(defmethod Array :collect (fcn)
	   (let ((result (send Array :new :size (send self :size))))
		(map-into (send result :data) fcn data)
		result))

; We also have to override :select, for the same reason

(defmethod Array :select (fcn)
	   (let ((result
		  (mapcan (lambda (x)
				  (when (funcall fcn x)
					(list x)))
			  (coerce (send self :data) 'list))))
		(send (send self :class) :new :initial result)))

; Finally, :storeon needs to be changed since :addAll doesn't work for
; arrays.	   

(defmethod Array :storeon ()
	   (list 'send
		 'Array
		 :new
		 :initial
		 (list
		  'quote
		  (send self :asList))))


; Test of the Array class:

; > (setq a (send x :asArray))              Make Array from Set x
; #<Array: 5 4 2 1>
; > (send a :atPut 1 10)                    Change an element
; #<Array: 5 10 2 1>
; > (send a :select #'evenp)                Get array of even elements
; #<Array: 10 2>
; > (send a :collect #'1+)                  Make array with values 1 larger
; #<Array: 6 11 3 2>             


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE ORDEREDCOLLECTION CLASS                            ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The OrderedCollection class uses linked lists and doesn't have the
; allocation problems of Array.
; Adding or deleteing from the start alters the index numbers,
; so we need a new instance variable to hold the offset.


(defclass OrderedCollection (offset) nil SequenceableCollection)

(defmethod OrderedCollection :isnew (&optional (offset 0))
	   ; Optional argument sets offset of first element.
	   ; This is a "private" feature to aid storeon.
	   (setf (send self :offset) offset))

; :at, :atPut, :first, and :last need revision

(defmethod OrderedCollection :at (arg) (elt data (+ arg offset)))

(defmethod OrderedCollection :atPut (arg value)
	   (setf (elt data (+ arg offset)) value)
	   self)

(defmethod OrderedCollection :first () (car data))

(defmethod OrderedCollection :last () (car (last data)))

; We need to implement add and remove for both ends
; :add will be equivalent to :addLast

(defmethod OrderedCollection :add (arg)
	   (setq data (nconc data (list arg)))
	   self)

(defmethod OrderedCollection :addlast (arg) (send self :add arg))

(defmethod OrderedCollection :addFirst (arg)
	   (setq offset (1+ offset))
	   (setq data (cons arg data))
	   self)

(defmethod OrderedCollection :removeFirst ()
	   (unless (zerop (length data))
		   (setq offset (1- offset))
		   (prog1 (car data) (setq data (cdr data)))))

(defmethod OrderedCollection :removeLast ()
	   (prog1 (car (last data)) (setq data (nbutlast data))))


; Finally, storeon is modified so that offset will be set


(defmethod OrderedCollection :storeon ()
	   (list 'send
		 (if (zerop offset)
		     (list 'send
			   (intern (send (send self :class) :pname))
			   :new)
		     (list 'send
			   (intern (send (send self :class) :pname))
			   :new
			   offset))
		 :addAll
		 (list
		  'quote
		  (send self :asList))))


; Example of use of OrderedCollection:


; > (setq  c (send a :asOrderedCollection))   Make one from Array a
; #<OrderedCollection: 5 10 2 1>
; > (send c :at 1)                            Value at index 1 is 10
; 10
; > (send c :addFirst 7)                      Add to front of collection
; #<OrderedCollection: 7 5 10 2 1>
; > (send c :at 1)                            Index 1 is same spot
; 10
; > (send c :removeLast)                      Remove from either end
; 1
; > (send c :last)                            Last element is now 2
; 2
; > (send c :asArray)                         Convert back to an array
; #<Array: 7 5 10 2>                        



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE SORTEDCOLLECTION CLASS                             ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; The SortedCollection class requires a sort function. The collection gets
; re-sorted whenever a new element is added. This is a subclass of
; OrderedCollection.

; Offset won't change for this class.	   


(defclass SortedCollection (sortfcn) nil OrderedCollection)

(defmethod SortedCollection :isnew (&optional (fcn '< ))	
	   (setq sortfcn fcn)
	   (send-super :isnew))

(defmethod SortedCollection :selfSort ()
	   ; "private" method that sorts the list
	   (setq data (sort data sortfcn))
	   self)

(defmethod SortedCollection :add (arg)
	   (send-super :add arg)
	   (send self :selfSort))

; Don't allow addFirst, addLast, removefirst, removelast, or atPut

(defmethod SortedCollection :addFirst (arg) 
	   (send self :notImplemented :addFirst))

(defmethod SortedCollection :addLast (arg) 
	   (send self :notImplemented :addLast))

(defmethod SortedCollection :removeFirst (arg) 
	   (send self :notImplemented :removeFirst))

(defmethod SortedCollection :removeLast (arg) 
	   (send self :notImplemented :removeLast))

(defmethod SortedCollection :atPut (arg1 arg2)
	   (send self :notImplemented :atPut))

; We need a way to remove elements from a Sorted Collection.
; :remove (specifying the element) will do just fine.

(defmethod SortedCollection :remove (arg)
	   (let ((pos (position arg data))) ; Find (first) instance
		(when pos ; Delete found element
		      (if (zerop pos)
			  (setq data (cdr data))
			  (setf (cdr (nthcdr (1- pos) data))
				(nthcdr (1+ pos) data))))
		self))


; Finally, storeon is modified so that the sort function will be set


(defmethod SortedCollection :storeon ()
	   (list 'send
		 (list 'send
		       (intern (send (send self :class) :pname))
		       :new
		       (list
			'quote
			sortfcn))
		 :addAll
		 (list
		  'quote
		  (send self :asList))))

; Let's see how the SortedCollection works:

; > (setq s (send c :asSortedCollection))    Sorted when it is created
; #<SortedCollection: 2 5 7 10>
; > (send s :add 8)                          :add puts new element in order
; #<SortedCollection: 2 5 7 8 10>
; > (send s :asSortedCollection #'>)         New collection with order reversed
; #<SortedCollection: 10 8 7 5 2>
; > (send (send (send Set :new) :addAll '(5 3 8 2 5 4 8)) :asSortedCollection)
; #<SortedCollection: 2 3 4 5 8>    Eliminate duplicates and sort
; > (send * :asList)
; (2 3 4 5 8)        


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;                                                               ;
;        THE INTERVAL CLASS                                     ;
;                                                               ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; The Interval class is considerably different than the others in that it has
; no "data" but calculates each elements value. We'll use three new
; instance variables -- start end and step. The :new function will take two
; or three arguments to specify start, end and step, with the step size
; defaulting to 1 not defined. We will set the data instance
; variable to T if the range is valid, and to NIL if not (no elements)


(defclass Interval (start end step) nil SequenceableCollection)

(defmethod Interval :isnew (arg1 arg2 &optional arg3)
	   (if arg3
	       (setq data (or (and (<= arg1 arg2)(> arg3 0))
			      (and (>= arg1 arg2)(< arg3 0))))
	       (setq arg3 1 
		     data (<= arg1 arg2)))
	   (setq start arg1 end arg2 step arg3)
	   ; Correct End value if necessary
	   (unless (zerop (rem (- end start) step))
		   (setq end (- end (rem (- end start) step))))
	   )

; :at calculates value. We won't check for out of range.

(defmethod Interval :at (arg) (+ start (* step arg)))

; :atPut isn't allowed

(defmethod Interval :atPut (arg1 arg2) (send self :notImplemented :atPut))

; :size returns calculated size

(defmethod Interval :size () (if data (1+ (truncate (- end start) step)) 0))

; :includes must be calcuated

(defmethod Interval :includes (arg)
	   (cond
	    ((null data) nil)
	    ((> step 0) (and (>= arg start)
			     (<= arg end)
			     (zerop (rem (- arg start) step))))
	    (t          (and (<= arg start)
			     (>= arg end)
			     (zerop (rem (- arg start) step))))))

; While Collection bases :asList on :map, we want to base :map on
; :asList

(defmethod Interval :map (fcn) (mapcar fcn (send self :asList)))

(defmethod Interval :asList ()
	   (let ((result nil))
		(when data
		      (dotimes (i (send self :size))
			       (setq result (cons (+ start (* i step))
						  result))))
		(nreverse result)))

; Since :do is used often with an Interval, and since the default method
; would create a list of values, it would make sense to reimplement :do
; here as an Interval method. That will be left as an exercise for the
; reader!

; :collect, :select will work because we will redefine :species to
; create an OrderedCollection rather than an Interval

(defmethod Interval :species () OrderedCollection)

; Override printing methods 

(defmethod Interval :prin1 (&optional (stream *standard-output*))
	   (format stream
		   "#<~a from ~s to ~s by ~s>"
		   (send (send self :class) :pname)
		   start end step))

; Override :storeon -- this one becomes really easy

(defmethod Interval :storeon ()
	   (list 'send 'Interval :new start end step))


; A few examples of the use of the Interval class:

; > (setq i (send Interval :new 2 10 2))        Make an interval, i
; #<Interval from 2 to 10 by 2>
; > (send i :do (lambda (x) (format t "~s " x)))  Demonstrate :do
; 2 4 6 8 10
; nil
; > (send i :at 3)                              Check operation of :at
; 8                  
; > (send i :size)                              Size of interval
; 5
; > (send i :asList)                            Convert to a list
; (2 4 6 8 10)
; > (send i :asSortedCollection #'>)            Convert to a SortedCollection
; #<SortedCollection: 10 8 6 4 2>               sequence changes!

(in-package "USER")       ; revert to default package
(use-package "CLASSES")    ; Make the classes package accessable
