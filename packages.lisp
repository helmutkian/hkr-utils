
;;; ***********************************************************************
;;; LAMBDA
;;;
;;; Function creation utilities
;;;
;;; ***********************************************************************
(defpackage :hkr.utils.lambda
  (:nicknames "hkr.lambda")
  (:use :common-lisp)
  (:import-from :metabang.utilities
		:flatten)
  (:import-from :hkr.utils.symbol
		:explode
		:symb)
  (:export :fn
	   :fn*))

;;; ***********************************************************************
;;; WITH-SELF
;;;
;;; Single dispatch-like implicit namespacing.
;;;
;;; ***********************************************************************
(defpackage :hkr.utils.clos.with-self
  (:nicknames "hkr.with-self")
  (:use common-lisp)
  (:import-from :metabang.utilities
		:flatten
		:mapappend)
  (:import-from :hkr.utils.clos.mop
		:with-readers
		:class-direct-slots)
  (:export :with-self
	   :defmethod-with-self))

;;; ***********************************************************************
;;; MOP
;;;
;;; MOP utilities
;;;
;;; ***********************************************************************
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

;;; ***********************************************************************
;;; SYMBOL
;;;
;;; Symbol utilities
;;;
;;; ***********************************************************************

(defpackage :hkr.utils.symbol
  (:nicknames "hkr.sym")
  (:use :common-lisp)
  (:export :symb
	   :explode
	   :separate-symbols))