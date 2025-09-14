(asdf:defsystem "genhash"
  :description "Generic hashtable code"
  :version "1.8"
  :licence "Public Domain"
  :author "Ingvar Mattsson <ingvar@hexapodia.net>"
  :maintainer "Paul Nathan <pnathan@alumni.uidaho.edu>"
  :components ((:file "packages")
	       (:file "genhash" :depends-on ("packages"))))
;;;
;; Note that there is also a "benchtests.lisp" that can be used to
;; test speed and consing for EQ, EQL, EQUAL and EQUALP tables.
;;;
