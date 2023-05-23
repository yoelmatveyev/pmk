(in-package :cl-pmk)

;; Closure that emulates the behavior of a MK-61 or B3-34 calculator

(declaim (inline print-indicator))

(defun print-indicator (screen commas)
  (declare (optimize (speed 3) (safety 0)(debug 0)))
  (let ((digitchars '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\- #\L #\C #\Г #\E #\Space)))
    (setf commas
	  (logior (ash (logand commas #x3FE) 2)
		  (logand (ash commas -10) #b111)))  
    (setf screen (logior
		  (ash (logand screen #xFFFFFFFFF) 12)
		  (ash (logand screen #xFFF000000000) -36))) 
    (format t "|")
    (loop for x from 0 to 11 do
      (write-char (nth (logand
			(ash screen (- (ash (- 11 x) 2))) #xF)
		       digitchars))
      (if (logbitp (- 11 x) commas) (format t ",")))
    (format t "|")))

(defun make-calc (&key (mk61 t) (mem 504))
  (declare (optimize (speed 3) (safety 0)(debug 0)))
  (let (screen (commas 0) (angle 10)
	       (irx (make-irx mem))
	       (ik1302 (make-ik1302))
	       (ik1303 (make-ik1303))
	       (ik1306 (make-ik1306))
	       (ind 0)
	       (oldscreen 0) (oldcommas 0)
	       (verbose nil) (nind t)
	       )
    (labels
	((run-tact ()
	   (let (tmp)
	     (multiple-value-bind (x y)
		 (funcall ik1302 :step (funcall irx :out t))
	       (setf tmp x ind y))
	     (setf tmp (funcall ik1303 :step tmp))
	     (if mk61 (setf tmp (funcall ik1306 :step tmp)))
	     (setf tmp (funcall irx :step tmp))
	     (funcall ik1302 :set-last tmp)))	   
	 (one-step ()
	   (funcall ik1303 :key (list angle 1))
	   (loop for i from 0 to 559 do
	     (loop for j from 0 to 41 do
	       (run-tact)
	       (when (plusp ind)
		 (multiple-value-bind (x y)
		     (funcall ik1302 :scr t)
		   (setf screen (logand x #xFFFFFFFFFFFF)
			 commas (logand y #x1FFE (if nind #x1DFF #x1FFF ))))  
		 (when (and (or (/= oldscreen screen) (/= oldcommas commas)) verbose)
		   (print-indicator screen commas)
		   (terpri))
		 (setf oldscreen screen oldcommas commas))))
	   (funcall ik1302 :key '(0 0)))
 	 
	 (set-zero ()
	   (setf screen nil commas nil angle 10)
	   (funcall irx :reset t)
	   (funcall ik1302 :reset t)
	   (funcall ik1303 :reset t)
	   (funcall ik1306 :reset t)))
      
      (lambda (&key steps press angle dump micro reset
		 verbon verboff norm-ind scr) 
	(cond
	  (steps (loop for j from 1 to steps do (one-step))
		 (values screen commas))
   	  (press (funcall ik1302 :key press)
		 (one-step)
		 (one-step))
	  (scr 	 (terpri)(print-indicator screen commas))
	  (angle (funcall ik1303 :key angle))
	  (dump (list
		 (funcall ik1302 :dump t)
		 (funcall ik1303 :dump t)
		 (if mk61 (funcall ik1306 :dump t))
		 (funcall irx :dump t)))
	  (reset (set-zero))
	  (norm-ind (setf nind norm-ind))
	  (verbon (setf verbose t))
	  (verboff (setf verbose nil))
	  (micro
	   (loop for x from 1 to micro do
	     (run-tact))))))))
