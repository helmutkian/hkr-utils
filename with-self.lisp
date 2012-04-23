
(in-package :hkr.utils.clos.with-self)

(defun collect-self-components (self body)
  "Collects the accessors, readers, and slots of SELF within a given method
body."
  (loop with flat-body = (flatten body)
        with self-slots = (class-direct-slots self)
        with self-accessors = (mapappend #'slot-definition-accessors self-slots)
        with self-readers = (mapappend #'slot-definition-readers self-slots)
       
        for x in flat-body

        if (find x self-accessors)
          collect x into accessors
        else if (find x self-readers)
          collect x into readers
        else if (find x self-slots)
          collect x into slots

        finally (return (list :accessors (remove-duplicates accessors)
			      :readers (remove-duplicates readers)
			      :slots (remove-duplicates slots)))))


(defmacro with-self ((self) &body body)
  (let* ((self-components
	  (collect-self-components (class-of (eval self)) body))
	 (accessors
	  (mapcar (lambda (x) (list x x)) (getf self-components :accessors)))
	 (readers
	  (mapcar (lambda (x) (list x x)) (getf self-components :readers)))
	 (slots
	  (mapcar (lambda (x) (list x x)) (getf self-components :slots))))
  `(with-accessors ,accessors ,self
     (with-readers ,readers ,self
       (with-slots ,slots ,self
	 ,@body)))))

(defmacro defmethod-with-self (name &body body)
  (let* ((combo (when (symbolp (first body)) (first body)))
	 (args (when combo (second body) (first body)))
	 (doc-string
	  (cond ((and combo (stringp (third body))) (third body))
		((stringp (second body)) (second body))
		(t nil)))
	 (meth-body
	  (cond ((and combo doc-string) (cdddr body))
		((or combo doc-string) (cddr body))
		(t (cdr body)))))
    `(defmethod ,@(remove nil (list name combo args doc-string))
	 ,@meth-body)))


