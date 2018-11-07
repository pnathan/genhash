(in-package "NET.HEXAPODIA.HASHTABLES")


(defvar *hash-test-designator-map* (make-hash-table))
(defvar *initialized* nil)

(define-condition hash-exists (simple-error) ()
  (:default-initargs
   :format-control "Hash table type ~a already registered"))

(define-condition unknown-hash (simple-error) ()
  (:default-initargs
   :format-control "Unknown hash table type ~a"))


(defclass hash-test-designator ()
  ((test-designator :reader test-designator :initarg :test-designator)
   (hash-function :reader hash-function :initarg :hash-function)
   (eq-test :reader eq-test :initarg :eq-test)
   (builtin :reader builtin :initarg :builtin :initform nil)))
   

(defclass hash-container ()
  ((buckets :accessor buckets :initarg :buckets)
   (allocated-buckets :accessor allocated-buckets :initarg :allocated-buckets)
   (used-buckets :accessor used-buckets :initform 0)
   (stored-items :accessor stored-items :initarg :stored-items) 
   (test-designator :reader test-designator :initarg :test-designator)))


(defun register-test-designator (test-designator hash-function equal-function)
  (when (gethash test-designator *hash-test-designator-map*)
    (let ((hashfun (gethash test-designator *hash-test-designator-map*)))
      (unless (and (eql hash-function (hash-function hashfun))
		   (eql equal-function (eq-test hashfun)))
	(restart-case
	    (error 'hash-exists :format-arguments (list test-designator))
	  (:unregister-and-retry ()
	   :report "Unregister hash table type and retry"
	    (remhash test-designator *hash-test-designator-map*)
	    (register-test-designator test-designator hash-function equal-function))))))
  
  (let ((hash-foo (make-instance 'hash-test-designator
				 :test-designator test-designator
				 :hash-function hash-function
				 :eq-test equal-function)))
    (setf (gethash test-designator *hash-test-designator-map*) hash-foo)))
(defun register-hash-function (test-designator hash-function equal-function)
  (register-test-designator test-designator hash-function equal-function))


(defun register-builtin (test-designator)
  (setf (gethash test-designator *hash-test-designator-map*)
	(make-instance 'hash-test-designator :builtin t)))

(defun make-generic-hash-table (&key (size 17) (test 'eql))
  (let ((test-designator test))
    (let ((nick (gethash test-designator *hash-test-designator-map*)))
      (unless nick
	(error 'unknown-hash :format-arguments (list test-designator)))
      
      (if (builtin nick)
	  (make-hash-table :test test-designator :size size) 
	(let ((storage (make-array (list size) :initial-element nil)))
	  (make-instance 'hash-container
			 :buckets storage
                         :stored-items 0
			 :allocated-buckets size
			 :test-designator test-designator))))))

(defun make-generic-hashtable (&key (size 17) (test 'eql))
  (make-generic-hash-table :size size :test test))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Add, get and remove values

(defgeneric hashref (key table &optional default))
(defgeneric (setf hashref) (value key table &optional default))
(defgeneric hashrem (key table))
(defgeneric hashmap (key table))


(defun expand-hash-table (table)
  (let* ((new-size (1+ (* 2 (allocated-buckets table))))
	 (new-buckets (make-array (list new-size) :initial-element nil)))
    (let ((old-data (buckets table)))
      (setf (allocated-buckets table) new-size)
      (setf (used-buckets table) 0)
      (setf (buckets table) new-buckets)
      (loop for bucket across old-data
	    do (loop for chunk in bucket
		     do (setf (hashref (car chunk) table) (cdr chunk))))))
  table)


(defmethod hashref (key (table hash-container) &optional default)
  (let ((hash-type (gethash (test-designator table) *hash-test-designator-map*)))
    (let ((hash (funcall (hash-function hash-type) key)))
      (let ((bucket
	     (aref (buckets table) (mod hash (allocated-buckets table)))))
	(let ((data default) found (eqfun (eq-test hash-type)))
	  (flet ((check (chunk)
		   (when (funcall eqfun (car chunk) key)
		     (setf data (cdr chunk))
		     (setf found t))))
	    (loop for chunk in bucket
		  until found
		  do (check chunk))
	    (values data found)))))))

(defmethod hashref (key (table hash-table) &optional default)
  (gethash key table default))


(defmethod (setf hashref) (value key (table hash-container) &optional def)
  (declare (ignore def))
  (let ((container (gethash (test-designator table) *hash-test-designator-map*)))
    (if (= (allocated-buckets table) (used-buckets table))
	(expand-hash-table table))

    (let ((hash (funcall (hash-function container) key))
	  (buckets (buckets table))
	  (size (allocated-buckets table)))
      (let* ((bucket-ix (mod hash size))
	     (bucket (aref buckets bucket-ix)))
	(if (null (aref buckets bucket-ix))
	    (progn
	      (setf (aref buckets bucket-ix)
		    (cons (cons key value) bucket))
	      (incf (used-buckets table))
	      (incf (stored-items table)))
	  (let ((check
		 (member key bucket
			 :key #'car :test (eq-test container))))
	    (if check
		(setf (cdr (car check)) value)
	      (progn
		(setf (aref buckets bucket-ix)
		    (cons (cons key value) bucket))
		(incf (stored-items table)))))))))
  value)

(defmethod (setf hashref) (value key (table hash-table) &optional default)
  (declare (ignore default))
  (setf (gethash key table) value))


(defmethod hashrem (key (table hash-container))
  (when (hashref key table nil)
    (let ((container (gethash (test-designator table) *hash-test-designator-map*)))
      (let ((hash (funcall (hash-function container) key))
	    (buckets (buckets table))
	    (size (allocated-buckets table)))
	(let* ((bucket-ix (mod hash size))
	       (bucket (aref buckets bucket-ix)))
	  (setf (aref buckets bucket-ix)
		(delete key bucket :test (eq-test container) :key 'car))
	  (unless (aref buckets bucket-ix)
	    (decf (used-buckets table)))
	  (decf (stored-items table)))))
    t))

(defmethod hashrem (key (table hash-table))
  (remhash key table))


(defmethod hashclr ((table hash-container))
  (setf (used-buckets table) 0)
  (loop for ix from 0 below (allocated-buckets table)
	do (setf (aref (buckets table) ix) nil))
  table)

(defmethod hashclr ((table hash-table))
  (clrhash table))

;;;;;;;;;;;;;;;;;;;;;;;
;;; Hash table iteration

(defmethod all-hash-keys ((table hash-container))
  (loop for list across (buckets table)
	append (mapcar #'car list)))
(defmethod all-hash-keys ((table hash-table))
  (loop for key being the hash-keys of table
	collect key))

(defmethod hashmap (fn (table hash-container))
  (let ((buckets (buckets table)))
    (loop for bucket across buckets
	  do (loop for chunk in bucket
		   do (funcall fn (car chunk) (cdr chunk))))))

(defmethod hashmap (fn (table hash-table))
  (maphash fn table))

(defmacro with-generic-hash-table-iterator ((name table) &body body)
  (let ((table-des (gensym "TABLE"))
	(the-keys (gensym "KEYS")))
    `(let ((,table-des ,table))
       (let ((,the-keys (all-hash-keys ,table-des)))
	 (macrolet ((,name ()
		     `(when ,the-keys
			  (prog1
			      (values t
				      (car ,the-keys)
				      (hashref (car ,the-keys) ,table-des))
			    (setf ,the-keys (cdr ,the-keys))))))
	   ,@body)))))


;;;;;;;;;;;;;;;;
;;; Hash table information
(defgeneric generic-hash-table-count (table))
(defgeneric generic-hash-table-size (table))
(defgeneric generic-hash-table-p (table))

(defmethod generic-hash-table-count ((table hash-container))
  (stored-items table))

(defmethod generic-hash-table-count ((table hash-table))
  (hash-table-count table))


(defmethod generic-hash-table-size ((table hash-container))
  (used-buckets table))

(defmethod generic-hash-table-size ((table hash-table))
  (hash-table-size table))


(defmethod generic-hash-table-p ((table t))
  nil)
(defmethod generic-hash-table-p ((table hash-container))
  t)
(defmethod generic-hash-table-p ((table hash-table))
  t)

;;;
;;; Setting up default hash tables
;;;
(unless *initialized*
  (setf *initialized* t)
  (register-test-designator 'eq     #'sxhash #'eq)
  (register-test-designator 'eql    #'sxhash #'eql)
  (register-test-designator 'equal  #'sxhash #'equal)
  (register-test-designator 'equalp #'sxhash #'equalp)
  (register-builtin #'eq)
  (register-builtin #'eql)
  (register-builtin #'equal)
  (register-builtin #'equalp))

