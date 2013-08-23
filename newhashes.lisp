(in-package "NET.HEXAPODIA.HASHTABLES")

(defun array-strings-hash (array)
  (let ((hashval 0))
    (loop for str across array
	  do (setf hashval (logxor hashval (sxhash str))))
    hashval))

(defun array-elements-equal (a b) 
  (loop for ae across a
	for be across b 
	always (equal ae be)))

	