
(defpackage :hkr.utils.clos.mop
  (:nicknames "hkr.mop")
  (:use :common-lisp
	:closer-mop)
  (:import-from :metabang.mop-utilities
		:get-class)
  (:export :slot-definition-accessors
	   :with-readers
	   :superclasses
	   :subclasses
	   :superclasses*
	   :subclasses*
	   :subclasses-intersection
	   :matching-superclasses-p
	   :find-subclass))

(in-package :hkr.utils.clos.mop)



;;; ***********************************************************************
;;; SLOT-DEFINITION-ACCESSORS
;;;
;;; Syntax: (slot-definition-accessors *slot*)
;;;         *slot* : a DIRECT-SLOT-DEFINITION object
;;;
;;; Like SLOT-DEFINITION-READERS or SLOT-DEFITION-WRITERS but instead
;;; returns a list of accessors, such that (*X0* *X1* ...) each *XN* has
;;; a reader #'*XN* and a SETF writer (setf *XN*).
;;; ***********************************************************************

(defun get-setf-field (sexpr)
  "Returns the second element of the form '(SETF X)"
  (when (eql (first sexpr) 'setf)
    (second sexpr)))

(defgeneric slot-definition-accessors (slot)
  "Returns a list of accessors for a DIRECT-SLOT-DEFINITION object"
  (:method ((slot direct-slot-definition))
    (intersection (slot-definition-readers slot)
		  (mapcar #'get-setf-field
			  (slot-definition-writers slot))))
  (:documentation "Returns a list of symbols naming the accessors of the
given slot"))

;;; ***********************************************************************
;;; WITH-READERS
;;;
;;; Syntax: (with-readers (*r0* ...) *object* *form*)
;;;         *r0* ... : a list of readers which can either be
;;;                    *reader* or (*renamed* *reader*)
;;;         *object* : the object whose readers are to be referenced
;;;         *form*   : an implicit PROGN
;;;
;;;
;;; Like WITH-SLOTS or WITH-ACCESSORS but for readers.
;;; ***********************************************************************

(defmacro with-readers (readers obj &body body)
"Like WITH-SLOTS or WITH-ACCESSORS, but for readers"
  `(let ,(mapcar
	  (lambda (r) (if (listp r)
		     `(,(first r) (,(second r) ,obj))
		     `(,r (,r ,obj))))
	  readers)
     ,@body))


;;; ***********************************************************************
;;; ***********************************************************************
(defun superclasses (thing)
  "Return a list of the direct superclasses of a class, object, or symbol
naming a class."
  (class-direct-superclasses (get-class thing)))

;;; ***********************************************************************
;;; ***********************************************************************

(defun subclasses (thing)
  "Return a list of the direct subclasses of a class, object, or symbol naming
a class"
  (class-direct-subclasses (get-class thing)))

;;; ***********************************************************************
;;; ***********************************************************************

(defun superclasses* (thing)
  "Return a list of all superclasses of a class, object, or symbol naming a
class and the superclasses of its superclasses all the way up to level where
STANDARD-CLASS or STANDARD-OBJECT isa superclass."
  (let ((sups (superclasses thing)))
    (unless (some (lambda (sup) (or (eql sup (find-class 'standard-object))
			       (eql sup (find-class 'standard-class))))
		  sups)
      (apply #'append sups (mapcar #'superclasses* sups)))))

;;; ***********************************************************************
;;; ***********************************************************************

(defun subclasses* (thing)
  "Return a list of all the subclasses of a class, object, or symbol naming a
class and the subclasses of all its subclasses."
  (let ((subs (subclasses thing)))
    (apply #'append subs (mapcar #'subclasses* subs))))

;;; ***********************************************************************
;;; ***********************************************************************

(defun subclasses-intersection% (c1 c2)
  "Returns the intersection of the SUPERCLASSES* of the SUBCLASSES* of C1 and the
SUPERCLASSES* of C2 "
  (apply #'hkrf.utils:intersection*
	 (mapcar #'remove-duplicates
		 (append (mapcar #'subclasses* (superclasses* c1))
			 (list (subclasses* c2))))))

(defun subclasses-intersection (c1 c2)
  (or (subclasses-intersection% c1 c2)
      (subclasses-intersection% c2 c1)))

;;; **********************************************************************
;;; **********************************************************************


(defun matching-superclasses-p (thing sups)
  (or (find (get-class thing) sups)
      (and (superclasses* thing)
	   (every (lambda (s) (matching-superclasses-p s sups))
		  (superclasses* thing)))))

;;; **********************************************************************
;;; **********************************************************************

(defun find-subclass (c1 c2)
  (loop with sups = (hkrf.utils:append* (get-class c1)
					(get-class c2)
					(superclasses* c1)
					(superclasses* c2))
       for sub in (subclasses-intersection c1 c2)
       for sub-sups = (set-difference (superclasses* sub) sups)
       if (every (lambda (s) (matching-superclasses-p s sups)) sub-sups)
         return sub))
