(in-package "NET.HEXAPODIA.HASHTABLES")

(defmacro emit-test (type count reffun &optional (size 17))
  `(time
    (funcall
     (compile nil
	      (lambda ()
		(format t "--------------------------------------------
Testing ~a hash-tables (~a elements), ~a~%" ',type ,count ,(if (eql reffun 'gethash) "Built-in" "User-defined"))
		(let ((table (,(if (eql reffun 'gethash)
				   'make-hash-table
				 'make-generic-hashtable)
			      :test ',type :size ,size)))
		  (loop for n from 0 to ,count
			do (setf (,reffun n table) n))))))))

;;;;;;
;;; Perform testing
;;;
(emit-test eq 1000 gethash)
(emit-test eq 1000 hashref)
(emit-test eq 100000 gethash)
(emit-test eq 100000 hashref)

(emit-test eql 1000 gethash)
(emit-test eql 1000 hashref)
(emit-test eql 100000 gethash)
(emit-test eql 100000 hashref)

(emit-test equal 1000 gethash)
(emit-test equal 1000 hashref)
(emit-test equal 100000 gethash)
(emit-test equal 100000 hashref)

(emit-test equalp 1000 gethash)
(emit-test equalp 1000 hashref)
(emit-test equalp 100000 gethash)
(emit-test equalp 100000 hashref)
