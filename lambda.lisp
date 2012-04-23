

(in-package :hkr.utils.lambda)

(defmacro fn (&body body)
  `(lambda (%) ,@body))


(defmacro fn* (&body body)
  (let* ((num-args
	  (loop for x in (flatten body)
	        count (and (symbolp x)
			   (eql (car (explode x)) '%))))
	 (args (loop for i from 1 to num-args
		     collect (symb '% i))))
    `(lambda ,args ,@body)))