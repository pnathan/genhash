(defpackage #:net.hexapodia.hashtables
  (:nicknames #:genhash)
  (:use #:cl)
  (:export #:make-generic-hash-table
	   #:make-generic-hashtable
	   #:register-hash-function
	   #:register-test-designator
	   #:hashref
	   #:hashrem
	   #:hashclr
	   #:hashmap
	   #:generic-hash-table-count
	   #:generic-hash-table-size
	   #:generic-hash-table-p
	   #:hash-exists
	   #:unknown-hash))
