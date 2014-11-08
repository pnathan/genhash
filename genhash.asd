(asdf:defsystem "genhash"
  :description "Generic hashtable code"
  :version "1.7"
  :components ((:file "packages")
	       (:file "genhash" :depends-on ("packages"))
	       ))
;;;
;; Note that there is also a "benchtests.lisp" that can be used to
;; test speed and consing for EQ, EQL, EQUAL and EQUALP tables.
;;;
