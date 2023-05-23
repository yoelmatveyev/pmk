(in-package :cl-pmk)

;; Simple auxiliary functions

(defun bitset (n pos b)
   "Sets bit index of an integer."
	   (if (plusp b)
	       (logior n (ash 1 pos))
	       (logand n (logxor (ash 1 pos) #b111111111111))))

;; Rotate an array by an index

(defun rotate-n (ar n)
  (loop for x from 0 to (1- (length ar)) collect (aref ar (mod (+ x n) (length ar))))) 
  
