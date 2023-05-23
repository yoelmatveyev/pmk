(in-package :cl-pmk)

;; Make a closure that emulates an IK13 microcontroller

(defun ik13 (com synch micro &optional chip)
  "Defines a closure that emulates an IK13 microcontroller"
  (declare (optimize (speed 3) (safety 0)(debug 0)))

  (let ((model chip) (commands com) (synchroprograms synch) (microcommands micro) m r st
	s s1 lflag tflag pflag
	alpha beta gamma sigma
	tact asp amc ac mode mic
	in out key-x key-y comma commas nflag
	(j (vector 0 1 2 3 4 5
		   3 4 5 3 4 5
		   3 4 5 3 4 5
		   3 4 5 3 4 5
		   6 7 8 0 1 2
		   3 4 5 6 7 8
		   0 1 2 3 4 5)))
    (labels
	(
	 ;; Reset the microcontroller 
	 (set-zero ()
	   (setf m (make-array #x2A :element-type 'fixnum)
		 r (make-array #x2A :element-type 'fixnum)
		 st (make-array #x2A :element-type 'fixnum)
		 s 0
		 s1 0
		 lflag 0
		 pflag 0
		 tflag 0
		 nflag 0
		 alpha 0
		 beta 0
		 gamma 0
		 sigma 0
		 tact 0
		 asp 0
		 amc 0
		 ac 0
		 mode 0
		 mic 0
		 key-x 0
		 key-y 0
		 commas 0))
	 
	 ;; Execute one microtact
	 
	 (run-tact ()
	   (let ((d9 (floor tact 9)) sum d3 r3)

	     (multiple-value-bind (x y) 
		 (floor tact 3)
	       (setf d3 x r3 y))

	     (when (zerop tact)
	       (setf ac (+ (ash (aref r 39) 4) (aref r 36)))
	       (if (zerop (logand (aref commands ac) #xFC0000))
		   (setf tflag 0)))

	     (cond ((< d9 3)
		    (setf asp (logand (aref commands ac) #xFF)))
		   ((= d9 3)
		    (setf asp (logand (ash (aref commands ac) -8) #xFF)))
		   ((= d9 4)
		    (setf asp (logand (ash (aref commands ac) -16) #xFF))
		    (when (> asp #x1F)
		      (if (= tact 36)
			  (setf (aref r 37) (logand asp #xF)
				(aref r 40) (ash asp -4)))
		      (setf asp #x5f))))
	     
	     (setf mode (logand (ash (aref commands ac) -24) #xFF)
		   amc (logand (aref synchroprograms (+ (* asp 9) (aref j tact))) #x3f))

	     (when (> amc 59)
	       (setf amc (+ (ash (- amc 60) 1) 60))
	       (if (zerop lflag) (incf amc)))

	     (setf mic (aref microcommands amc)
		   alpha 0
		   beta 0
		   gamma 0)

	     (if (and (plusp (logand (ash mic 25) 1))
		      (/= d3 (1- key-x)))
		 (setf s1 (logior s1 key-y)))
	     
	     (if (plusp (logand mic #b000000000001))
		 (setf alpha (logior alpha (aref r tact))))
	     (if (plusp (logand mic #b000000000010))
		 (setf alpha (logior alpha (aref m tact))))
	     (if (plusp (logand mic #b000000000100))
		 (setf alpha (logior alpha (aref st tact))))
	     (if (plusp (logand mic #b000000001000))
		 (setf alpha (logior alpha (logand (lognot (aref r tact)) #xF))))
	     (if (plusp (logand mic #b000000010000))
		 (if (zerop lflag) (setf alpha (logior alpha #xA))))
	     (if (plusp (logand mic #b000000100000))
		 (setf alpha (logior alpha s)))
	     (if (plusp (logand mic #b000001000000))
		 (setf alpha (logior alpha 4)))
	     
	     (if (plusp (logand mic #b000010000000))
		 (setf beta (logior beta s)))
	     (if (plusp (logand mic #b000100000000))
	 	 (setf beta (logior beta (logand (lognot s) #xF))))
	     (if (plusp (logand mic #b001000000000))
		 (setf beta (logior beta s1)))
	     (if (plusp (logand mic #b010000000000))
		 (setf beta (logior beta 6)))
	     (if (plusp (logand mic #b100000000000))
		 (setf beta (logior beta 1)))

	     (if (plusp (logand (aref commands ac) #xFC0000))
		 (if (zerop key-y) (setf tflag 0))
		 (progn
		   (if (and (= d3 (1- key-x)) (plusp key-y))
		       (setf s1 key-y tflag 1))
		   (if (< d3 12)
		       (if (plusp lflag) (setf comma d3)))
		   (if (plusp lflag)
		       (setf commas (logior commas (ash 1 d3)))
		       (setf commas (logand commas (logxor (ash 1 d3) #b11111111111111))))
		   (setf nflag 1)))
	     
	     (if (plusp (logand mic #b001000000000000))
		 (setf gamma (logior gamma lflag)))
	     (if (plusp (logand mic #b010000000000000))
		 (setf gamma (logior gamma (logand (lognot lflag) 1))))
	     (if (plusp (logand mic #b100000000000000))
		 (setf gamma (logior gamma (logand (lognot tflag) 1))))

	     (setf sum (+ alpha beta gamma)
		   sigma (logand sum #xF)
		   pflag (logand (ash sum -4) 1))

	     (when (or (zerop mode) (>= tact 36))
	       (case (logand (ash mic -15) #b111)
		 (1 (setf (aref r tact) (aref r (mod (+ tact 3) 42))))
		 (2 (setf (aref r tact) sigma))
		 (3 (setf (aref r tact) s))
		 (4 (setf (aref r tact) (logior (aref r tact) s sigma)))
		 (5 (setf (aref r tact) (logior s sigma)))
		 (6 (setf (aref r tact) (logior (aref r tact) s)))
		 (7 (setf (aref r tact) (logior (aref r tact) sigma))))
	       (if (plusp (logand 1 (ash mic -18))) (setf (aref r (mod (1- tact) 42)) sigma))
	       (if (plusp (logand 1 (ash mic -19))) (setf (aref r (mod (- tact 2) 42)) sigma)))

	     (if (plusp (logand (ash mic -20) 1)) (setf (aref m tact) s))
	     (if (plusp (logand (ash mic -21) 1)) (setf lflag pflag))

	     (case (logand (ash mic -22) #b11)
	       (1 (setf s s1))
	       (2 (setf s sigma))
	       (3 (setf s1 (logior s1 sigma))))
	     
	     (case (logand (ash mic -24) #b11)
	       (1 (setf s1 sigma))
					; (2 (setf s1 s1))
					; This opcode is used for input (keyboard/angle in MK-61) 

	       (3 (setf s1 (logior s1 sigma))))

	     (let (x y z)
	       (case (logand (ash mic -26) #b11)
		 (1 (setf (aref st (mod (+ tact 2) 42)) (aref st (mod (+ tact 1) 42))
			  (aref st (mod (+ tact 1) 42)) (aref st tact)
			  (aref st tact) sigma))
		 (2 (setf x (aref st tact)
			  (aref st tact) (aref st (mod (+ tact 1) 42))
			  (aref st (mod (+ tact 1) 42)) (aref st (mod (+ tact 2) 42))
			  (aref st (mod (+ tact 2) 42)) x))
		 (3 (setf x (aref st tact)
			  y (aref st (mod (+ tact 1) 42))
			  z (aref st (mod (+ tact 2) 42))
	     	          (aref st tact) (logior sigma y)
			  (aref st (mod (+ tact 1) 42))(logior x z)
			  (aref st (mod (+ tact 2) 42))(logior y x)))))

	     ;; Finalizing the microtact

	     (setf out (aref m tact)
		   (aref m tact) in)
	     (incf tact)
	     (if (> tact 41) (setf tact 0))
	     (values out nflag)))
	 
	 (dump ()
	   (list (list m r st) (list s s1)
		 (list lflag pflag tflag nflag commas)
		 (list key-x key-y asp tact mode)))

	 (dump-rom ()
	   (list commands synchroprograms microcommands model))
	 
	 (scr ()
	   (setf nflag 0)
	   (values
	    (let ((sc 0))
	     (loop for x from 0 to 13 do
	       (setf sc (logior sc (ash (aref r (* x 3)) (ash x 2)))))
	      sc)
	    commas
	    comma)))
	   	   
    (set-zero)

      ;; The "set-last" key is used to emulate the ring structure of the microcontroller.
      ;; It sets the previous element of the circular bus memory 
     
    (lambda (&key step dump reset dump-rom out set-last key scr)
      (cond (step (progn
		 (setf in (if (numberp step) step 0))
		 (run-tact)))
	    (dump (dump))
	    (reset (set-zero))
	    (dump-rom (dump-rom))
	    (out (aref m tact))
	    (set-last (setf (aref m (mod (1- tact) 42)) set-last))
	    (key (setf key-x (car key) key-y (cadr key)))
	    (scr (scr)))))))

;; 8bit+8bit+8bit+1bit command packing is used, easier for human reading
;; Some emulators use denser 7bit+7bit+8bit+1 packing of IK13 ROMs

(defun make-ik1302 ()
  (declare (optimize (speed 3) (safety 0)(debug 0)))
  (ik13 
   (make-array #x100 :element-type 'fixnum :initial-contents 
	       '(#x00204E4E #x00117360 #x00114840 #x01040240
		 #x00164040 #x001B3240 #x00064640 #x015B4013
		 #x00D93130 #x00001040 #x01A52014 #x00000000
		 #x00000000 #x00000000 #x00000000 #x00C12040
		 #x00D0536D #x00517740 #x00B43130 #x00B22223
		 #x00C15340 #x00FD2040 #x002D1D1D #x0008403B
		 #x00092140 #x00094061 #x000A2140 #x00082140
		 #x000D7076 #x010D400D #x000A403B #x00056D40
		 #x00100259 #x010B1340 #x00242044 #x010B7840
		 #x00064002 #x01FF2008 #x0008565A #x0126403F
		 #x016C400D #x00C12077 #x00517740 #x00517740
		 #x00083240 #x010C400D #x01FF200A #x010B3568
		 #x00117B5A #x0021206D #x01222034 #x01015C5B
		 #x01D03454 #x00005E5D #x010E400D #x010E0044
		 #x00F44E40 #x009A206D #x00F44E5A #x00000000
		 #x00000000 #x00000000 #x00000000 #x00C11D1D
		 #x00063333 #x010B403B #x01344043 #x00096A6A
		 #x000A4443 #x00792120 #x01D32047 #x00081E1E
		 #x01AF1140 #x00AB1D1D #x0039324C #x000B324C
		 #x0008326D #x000D404C #x00854D40 #x00134040
		 #x0009404C #x006D7770 #x006D7240 #x01001640
		 #x00A54C7E #x00F44E40 #x01536900 #x000A580E
		 #x003C5262 #x0005716D #x013C4013 #x00104070
		 #x00056F6D #x00A62070 #x00106F40 #x01056F40
		 #x001F3E3D #x0028595A #x001E2223 #x00064B40
		 #x00524A40 #x00692120 #x001B4940 #x00093240
		 #x011F0140 #x00154840 #x00062423 #x00062423
		 #x01057340 #x015E400D #x00095828 #x00092223
		 #x00992F40 #x00982F40 #x00622040 #x005D5820
		 #x00740F40 #x00B81C20 #x00D05373 #x005B205C
		 #x006D2062 #x0133200A #x010B7D62 #x00A52120
		 #x01054072 #x01494013 #x01040540 #x00217362
		 #x013D6A40 #x00067840 #x01AB6C6D #x01332014
		 #x000E7C6C #x00050B3F #x00C15340 #x00950853
		 #x00E0417A #x00E04240 #x00532120 #x00365562
		 #x008F1E20 #x013D1740 #x004C2120 #x0170406A
		 #x00C05340 #x00061D1D #x00814545 #x00063333
		 #x00061E1E #x00091E1E #x00900720 #x01514078
		 #x00081D1D #x01622206 #x001E4545 #x00114060
		 #x000B2E40 #x000F2D40 #x010E1F40 #x000D7677
		 #x00D33C40 #x01D32032 #x00116E60 #x011D3440
		 #x00FF7440 #x00073240 #x001B430A #x01D32047
		 #x00113434 #x001E6E40 #x00D33C40 #x00937540
		 #x00D01E20 #x00043277 #x00CA4020 #x00107F54
		 #x00212068 #x000B7840 #x017C400C #x00056F6D
		 #x01470C40 #x01716B62 #x006B2120 #x00332120
		 #x006D204C #x00E67362 #x010D0940 #x00062423
		 #x001A3A3A #x018F406F #x0151334C #x010D1716
		 #x01D35340 #x00D24061 #x00CA6554 #x00104064
		 #x00512223 #x00782120 #x00263130 #x001E3434
		 #x00193838 #x00183939 #x000D6654 #x010D7A40
		 #x010E1740 #x00057340 #x00B86140 #x00045263
		 #x00122773 #x008F5373 #x002E5150 #x0151404C
		 #x001E3737 #x00894E40 #x001E3636 #x006D563D
		 #x00E07A41 #x00E12973 #x00082640 #x00062540
		 #x00D87967 #x0005565A #x0005286C #x00762041
		 #x00952040 #x008F1D1D #x01D35340 #x008F2040
		 #x00CC4F4F #x00114060 #x00054040 #x001E3434
		 #x01047340 #x011E3434 #x00C62C2B #x00C53130
		 #x003E1D1D #x01041740 #x001E3535 #x00D35353
		 #x00DE4077 #x00E24057 #x00064E68 #x01E53812
		 #x00D84067 #x00064069 #x000A402A #x00EF202A
		 #x01015C5B #x00090F40 #x00005E5D #x010B3613
		 #x00144740 #x01176806 #x000A5A5A #x01D3200D))

   (make-array #x480 :element-type 'fixnum :initial-contents 
	       '(#x00  #x00  #x00  #x10  #x03  #x1D  #x00  #x07  #x1E
		 #x10  #x03  #x1C  #x0B  #x07  #x0C  #x1E  #x00  #x00 
		 #x15  #x18  #x09  #x16  #x18  #x09  #x16  #x18  #x24
		 #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00
		 #x00  #x03  #x0E  #x1E  #x33  #x00  #x00  #x00  #x00 
		 #x23  #x00  #x00  #x00  #x2F  #x00  #x2C  #x00  #x01
		 #x11  #x32  #x00  #x00  #x00  #x03  #x00  #x0E  #x1A
		 #x0F  #x0E  #x0D  #x19  #x03  #x2F  #x0E  #x0D  #x08 
		 #x1C  #x0C  #x0D  #x01  #x00  #x00  #x03  #x24  #x0F
		 #x1C  #x0C  #x2F  #x09  #x1E  #x34  #x0E  #x1E  #x0C 
		 #x06  #x0A  #x0D  #x00  #x00  #x00  #x00  #x09  #x0F
		 #x38  #x00  #x00  #x00  #x00  #x00  #x0A  #x26  #x06
		 #x35  #x34  #x0D  #x24  #x1E  #x1A  #x09  #x0C  #x0F 
		 #x3D  #x00  #x00  #x1C  #x03  #x0E  #x0A  #x0F  #x06
		 #x3D  #x00  #x0E  #x3F  #x03  #x01  #x00  #x00  #x0E
		 #x3F  #x33  #x0D  #x01  #x08  #x00  #x01  #x08  #x04
		 #x06  #x03  #x0E  #x2B  #x3A  #x09  #x12  #x1E  #x33 
		 #x35  #x03  #x07  #x0C  #x1E  #x1A  #x00  #x00  #x00
		 #x35  #x0C  #x2F  #x0E  #x03  #x01  #x00  #x00  #x15
		 #x24  #x1E  #x1A  #x23  #x1D  #x00  #x00  #x00  #x00
		 #x09  #x0C  #x2F  #x09  #x03  #x00  #x24  #x0C  #x0F
		 #x3D  #x09  #x1E  #x3F  #x03  #x07  #x0B  #x22  #x03
		 #x07  #x0B  #x0D  #x0C  #x03  #x0E  #x1E  #x3A  #x2B 
		 #x3C  #x03  #x00  #x09  #x34  #x0E  #x1E  #x0C  #x1E
		 #x2E  #x01  #x31  #x2E  #x01  #x31  #x00  #x00  #x00
		 #x2E  #x30  #x03  #x2E  #x30  #x03  #x00  #x00  #x00
		 #x2E  #x2D  #x00  #x2E  #x2D  #x00  #x00  #x00  #x00 
		 #x3B  #x04  #x2F  #x37  #x12  #x00  #x00  #x00  #x00
		 #x14  #x00  #x00  #x08  #x00  #x00  #x00  #x00  #x00
		 #x01  #x13  #x00  #x01  #x13  #x00  #x01  #x13  #x04
		 #x2E  #x00  #x00  #x2E  #x00  #x00  #x2E  #x00  #x00 
		 #x3D  #x07  #x10  #x3F  #x03  #x00  #x2C  #x07  #x1E 
		 #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x04
		 #x00  #x0F  #x10  #x03  #x00  #x1C  #x03  #x0F  #x1D 
		 #x03  #x32  #x00  #x2B  #x14  #x00  #x00  #x08  #x00
		 #x04  #x14  #x00  #x00  #x32  #x00  #x00  #x32  #x0C
		 #x0A  #x32  #x00  #x00  #x32  #x00  #x00  #x32  #x00
		 #x21  #x15  #x18  #x21  #x16  #x18  #x00  #x17  #x18
		 #x19  #x1A  #x18  #x19  #x16  #x18  #x09  #x16  #x18 
		 #x2B  #x15  #x00  #x00  #x17  #x00  #x00  #x17  #x00
		 #x12  #x1B  #x0E  #x0F  #x1B  #x0E  #x23  #x2B  #x0A
		 #x2C  #x18  #x00  #x2A  #x18  #x07  #x0B  #x03  #x04
		 #x32  #x14  #x00  #x32  #x32  #x11  #x00  #x08  #x00
		 #x09  #x0C  #x15  #x03  #x00  #x00  #x06  #x3C  #x00 
		 #x00  #x2C  #x00  #x00  #x2A  #x00  #x09  #x16  #x00
		 #x00  #x00  #x11  #x00  #x09  #x16  #x18  #x09  #x1E
		 #x00  #x00  #x07  #x0A  #x29  #x3E  #x33  #x29  #x00 
		 #x0F  #x0B  #x0F  #x10  #x03  #x08  #x24  #x03  #x23
		 #x32  #x01  #x1D  #x32  #x08  #x00  #x32  #x08  #x32
		 #x32  #x08  #x23  #x32  #x08  #x0F  #x23  #x23  #x04
		 #x09  #x1E  #x0F  #x00  #x00  #x14  #x00  #x00  #x08 
		 #x37  #x00  #x00  #x37  #x00  #x00  #x37  #x00  #x00
		 #x01  #x31  #x00  #x01  #x31  #x00  #x01  #x31  #x36
		 #x1A  #x30  #x0D  #x00  #x30  #x0D  #x00  #x30  #x0D
		 #x30  #x03  #x00  #x30  #x03  #x00  #x30  #x03  #x2B
		 #x2D  #x00  #x00  #x2D  #x00  #x00  #x2D  #x00  #x00 
		 #x0A  #x30  #x03  #x00  #x30  #x03  #x00  #x30  #x03
		 #x00  #x01  #x31  #x00  #x01  #x31  #x00  #x01  #x31
		 #x00  #x2D  #x00  #x00  #x2D  #x00  #x00  #x2D  #x00
		 #x2C  #x00  #x00  #x2A  #x00  #x00  #x09  #x18  #x00
		 #x07  #x1E  #x0F  #x01  #x00  #x08  #x1C  #x0A  #x08 
		 #x14  #x00  #x00  #x32  #x00  #x00  #x32  #x2B  #x00
		 #x32  #x00  #x00  #x32  #x27  #x36  #x08  #x09  #x0C
		 #x1E  #x02  #x1D  #x0F  #x0C  #x0F  #x26  #x07  #x22 
		 #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x00
		 #x1D  #x23  #x23  #x09  #x23  #x0C  #x03  #x23  #x23 
		 #x02  #x35  #x03  #x0F  #x00  #x00  #x00  #x04  #x0C
		 #x01  #x12  #x00  #x08  #x00  #x32  #x0A  #x00  #x00
		 #x06  #x18  #x00  #x17  #x18  #x00  #x17  #x18  #x00
		 #x00  #x01  #x13  #x00  #x01  #x13  #x04  #x01  #x13
		 #x00  #x00  #x00  #x09  #x15  #x18  #x00  #x35  #x03
		 #x0E  #x03  #x09  #x0C  #x1B  #x1E  #x0F  #x1B  #x08 
		 #x00  #x00  #x1C  #x03  #x1E  #x15  #x02  #x0C  #x00
		 #x07  #x1E  #x10  #x0F  #x09  #x32  #x1E  #x0F  #x08
		 #x09  #x1E  #x1A  #x18  #x1D  #x17  #x03  #x0F  #x3D 
		 #x07  #x0B  #x1A  #x1D  #x28  #x00  #x0E  #x28  #x08
		 #x00  #x00  #x00  #x00  #x00  #x06  #x03  #x00  #x09
		 #x00  #x04  #x2B  #x23  #x04  #x08  #x08  #x00  #x08 
		 #x0E  #x03  #x00  #x2B  #x2F  #x0D  #x12  #x03  #x04 
		 #x01  #x08  #x00  #x01  #x08  #x00  #x01  #x08  #x04
		 #x0F  #x1D  #x2F  #x0E  #x03  #x23  #x07  #x1E  #x0D 
		 #x0F  #x12  #x00  #x23  #x24  #x1E  #x23  #x0F  #x04
		 #x26  #x12  #x15  #x03  #x12  #x04  #x24  #x2F  #x0F
		 #x12  #x04  #x01  #x0F  #x07  #x1E  #x0F  #x00  #x01 
		 #x0E  #x0F  #x20  #x05  #x00  #x07  #x12  #x0E  #x08
		 #x1E  #x00  #x10  #x03  #x0F  #x04  #x00  #x00  #x00
		 #x32  #x00  #x00  #x32  #x00  #x00  #x00  #x03  #x00
		 #x00  #x00  #x01  #x00  #x00  #x05  #x00  #x17  #x0D 
		 #x00  #x00  #x00  #x0A  #x1A  #x18  #x00  #x17  #x03
		 #x32  #x09  #x0F  #x32  #x07  #x0C  #x0C  #x1A  #x0F 
		 #x14  #x00  #x00  #x32  #x00  #x00  #x32  #x00  #x00
		 #x0E  #x1E  #x15  #x00  #x00  #x02  #x00  #x00  #x02 
		 #x00  #x0E  #x08  #x0E  #x1D  #x23  #x1E  #x3A  #x3A 
		 #x1D  #x04  #x15  #x00  #x00  #x3A  #x00  #x00  #x3A
		 #x00  #x00  #x3A  #x00  #x0D  #x0E  #x03  #x0F  #x00
		 #x3B  #x3C  #x2F  #x37  #x3C  #x01  #x00  #x00  #x00
		 #x00  #x00  #x00  #x00  #x30  #x00  #x02  #x24  #x1E
		 #x00  #x00  #x00  #x00  #x07  #x0B  #x22  #x03  #x04
		 #x00  #x00  #x39  #x04  #x25  #x08  #x03  #x07  #x0F
		 #x12  #x2C  #x00  #x2B  #x2A  #x26  #x0D  #x07  #x0F 
		 #x04  #x0B  #x08  #x01  #x10  #x0D  #x09  #x00  #x00
		 #x00  #x01  #x08  #x04  #x01  #x08  #x23  #x01  #x08
		 #x00  #x00  #x1B  #x00  #x00  #x1B  #x1F  #x0E  #x1B 
		 #x00  #x00  #x00  #x00  #x00  #x2C  #x00  #x1B  #x00
		 #x00  #x00  #x01  #x0F  #x0D  #x01  #x09  #x1E  #x2B 
		 #x00  #x23  #x1A  #x07  #x1E  #x0C  #x0F  #x00  #x00
		 #x1E  #x12  #x00  #x00  #x12  #x00  #x00  #x12  #x1A
		 #x1E  #x00  #x10  #x0F  #x24  #x1E  #x34  #x1D  #x00 
		 #x02  #x00  #x00  #x00  #x00  #x00  #x09  #x2F  #x01
		 #x00  #x00  #x00  #x00  #x00  #x00  #x12  #x09  #x15
		 #x00  #x00  #x00  #x00  #x00  #x28  #x00  #x00  #x28
		 #x00  #x00  #x2B  #x00  #x00  #x00  #x09  #x0C  #x23 
		 #x24  #x0C  #x1E  #x0F  #x00  #x07  #x03  #x0F  #x00 
		 #x00  #x00  #x01  #x0F  #x07  #x0B  #x0F  #x25  #x0F
		 #x0F  #x04  #x00  #x00  #x00  #x12  #x09  #x0C  #x12
		 #x00  #x00  #x00  #x12  #x00  #x00  #x00  #x09  #x0C
		 #x03  #x00  #x00  #x00  #x04  #x32  #x24  #x0F  #x23
		 #x0E  #x0D  #x00  #x00  #x00  #x00  #x09  #x1E  #x1A
		 #x07  #x0B  #x0F  #x07  #x0C  #x1E  #x1A  #x0F  #x00
		 #x0E  #x0D  #x00  #x00  #x00  #x00  #x00  #x04  #x08
		 #x12  #x00  #x01  #x0B  #x00  #x00  #x00  #x00  #x09
		 #x00  #x00  #x00  #x00  #x00  #x00  #x00  #x06  #x0B
		 #x00  #x00  #x12  #x00  #x00  #x12  #x04  #x0C  #x12
		 #x32  #x00  #x00  #x32  #x00  #x00  #x08  #x36  #x00
		 #x02  #x0D  #x00  #x01  #x0F  #x0D  #x00  #x0E  #x1E 
		 #x1E  #x00  #x10  #x0F  #x07  #x0B  #x34  #x0F  #x1D
		 #x1D  #x04  #x08  #x36  #x00  #x08  #x12  #x00  #x00
		 #x03  #x1E  #x0F  #x26  #x0A  #x02  #x26  #x3E  #x08))
   
   (make-array #x44 :element-type 'fixnum :initial-contents 
	       '(#x0000000 #x0800001 #x0A00820 #x0040020
		 #x0A03120 #x0203081 #x0A00181 #x0803800
		 #x0818001 #x0800400 #x0A00089 #x0A03C20
		 #x0800820 #x0080020 #x0800120 #x1400020
		 #x0800081 #x0210801 #x0040000 #x0058001
		 #x0808001 #x0A03081 #x0A01081 #x0A01181
		 #x0040090 #x0800401 #x0A00081 #x0040001
		 #x0800801 #x1000000 #x0800100 #x1200801
		 #x0013C01 #x0800008 #x0A00088 #x0010200
		 #x0800040 #x0800280 #x1801200 #x1000208
		 #x0080001 #x0A00082 #x0A01008 #x1000001
		 #x0A00808 #x0900001 #x8010004 #x0080820
		 #x0800002 #x0140002 #x0008000 #x0A00090
		 #x0A00220 #x0801001 #x1203200 #x4800001
		 #x0011801 #x1008001 #x0A04020 #x4800801
		 #x0840801 #x0840020 #x0013081 #x0010801
		 #x0818180 #x0800180 #x0A00081 #x0800001))
   2))

(defun make-ik1303 ()
  (declare (optimize (speed 3) (safety 0)(debug 0)))
  (ik13 
   (make-array #x100 :element-type 'fixnum :initial-contents 
	       '(#x00386050 #x005B3F3E #x000F5970 #x00152470
		 #x000C3D50 #x0011312F #x005B4544 #x00165050
		 #x000C3404 #x005B3F3E #x00D40450 #x00162424
		 #x000C4962 #x01FB5250 #x000D4924 #x01BB2222
		 #x00155050 #x010F5247 #x00182525 #x00080505
		 #x000E041E #x00123433 #x007F6425 #x007F0D25
		 #x01650950 #x01176553 #x007E2432 #x00087150
		 #x007E2455 #x00135076 #x00085977 #x005B4544
		 #x000C2E26 #x00310D2E #x00100E35 #x00316B47
		 #x01381250 #x0011302E #x01385F50 #x00050250
		 #x011C0101 #x00195050 #x00382C2C #x016F2222
		 #x013A2222 #x002F6B56 #x00093D6C #x00F04D50
		 #x000C1750 #x00074A50 #x01B45047 #x003C2020
		 #x01AA2B6A #x00123432 #x001D4933 #x0113500C
		 #x00052556 #x00087C50 #x01130000 #x00142B2B
		 #x004A1D50 #x006E5756 #x00496050 #x00E57D58
		 #x011E5D22 #x01F35F50 #x00EA0505 #x001C7A50
		 #x01080B50 #x0054244B #x000C4050 #x002A2121
		 #x00135C5C #x000A4650 #x00152504 #x009D2B60
		 #x00064350 #x00192020 #x00292C2C #x01235C50
		 #x006D3C3C #x0031017D #x00092D2D #x004E2D2D
		 #x01596A7E #x00E3396E #x006E3654 #x016E6E47
		 #x00534950 #x00EE2062 #x0016226E #x00660525
		 #x00135C5C #x000A4241 #x00383B3B #x000C7277
		 #x00360404 #x00042020 #x00100A2E #x00155050
		 #x00532404 #x0004642B #x01843C47 #x01A35047
		 #x01847250 #x015C112F #x00080434 #x00152F23
		 #x00080505 #x00906047 #x0113150C #x006D2224
		 #x00747250 #x000C632B #x00AD672B #x000A612E
		 #x01B97463 #x00417374 #x00BD0658 #x00EA2450
		 #x00087166 #x01BD3950 #x001A2E50 #x00BD6047
		 #x00175079 #x005E6035 #x000A3847 #x01067F47
		 #x008C5251 #x0013612E #x0087602E #x005B3F3E
		 #x00DC2121 #x00177374 #x00182525 #x00286050
		 #x00064F4E #x000C5251 #x006E2926 #x008F602F
		 #x008C502A #x00172928 #x00814F4E #x003F534B
		 #x000F075B #x00082525 #x01E85047 #x00790505
		 #x00152F23 #x0017506A #x00095047 #x00082525
		 #x00E63A62 #x00DA0B47 #x01174150 #x00182525
		 #x00090450 #x01175B50 #x00094850 #x001B2F50
		 #x00806047 #x000A3720 #x0010382F #x002C0505
		 #x009B5021 #x00160505 #x01ED3A50 #x00040505
		 #x00082525 #x01080F50 #x01B35047 #x000D3D4C
		 #x00180404 #x01C03A50 #x00E20421 #x00287B50
		 #x00097F26 #x0013612E #x01B6112F #x00322425
		 #x01B81847 #x00BA714B #x00182450 #x00080505
		 #x00182525 #x004F1D24 #x00736F5C #x00A67569
		 #x00AD2726 #x01BE5022 #x000A5E04 #x00173A62
		 #x00CB752E #x00B11E25 #x00CB0953 #x00085068
		 #x002B2020 #x01984150 #x00C77C04 #x00DA0950
		 #x00160404 #x00F56040 #x00DE0450 #x01CB1160
		 #x00CF4950 #x000A4747 #x001F210B #x00145050
		 #x01171050 #x00052075 #x001D3D37 #x00365555
		 #x00130101 #x01D57424 #x00D66047 #x01C47850
		 #x004D2C2C #x01174150 #x00174847 #x00C90350
		 #x000A2760 #x0019502E #x00D72C2C #x01174850
		 #x006C224B #x000A495B #x00100E35 #x00312104
		 #x01C00850 #x00115A2F #x00EA0505 #x00080574
		 #x00152F23 #x005C6050 #x01C94122 #x01A42222
		 #x00DF2847 #x00C9202E #x00A76047 #x0117502F
		 #x002E2020 #x01205048 #x00F8606D #x002D604C
		 #x00443A62 #x000D3D2E #x015C3950 #x01625022
		 #x006E136E #x0031602E #x01085D1A #x010F6F50
		 #x0017506A #x00FB5020 #x000A3C47 #x00174D50))

   (make-array #x480 :element-type 'fixnum :initial-contents 
	       '(#x2C #x23 #x00 #x2C #x23 #x00 #x2C #x23 #x30
		 #x31 #x32 #x00 #x31 #x32 #x12 #x31 #x32 #x30
		 #x00 #x00 #x00 #x11 #x23 #x00 #x1F #x06 #x00
		 #x31 #x00 #x1C #x31 #x00 #x00 #x31 #x08 #x1D
		 #x2C #x02 #x0E #x2C #x02 #x01 #x2C #x02 #x08
		 #x08 #x3A #x00 #x00 #x3A #x01 #x05 #x3A #x11
		 #x18 #x0A #x2B #x00 #x01 #x33 #x02 #x24 #x25
		 #x37 #x3A #x18 #x31 #x3A #x1F #x31 #x3A #x3D
		 #x37 #x02 #x06 #x31 #x02 #x12 #x31 #x10 #x19
		 #x39 #x02 #x26 #x33 #x09 #x08 #x19 #x19 #x08
		 #x01 #x14 #x0C #x00 #x00 #x00 #x1B #x06 #x01
		 #x26 #x00 #x21 #x12 #x14 #x24 #x06 #x12 #x00
		 #x39 #x00 #x21 #x08 #x22 #x00 #x10 #x14 #x00
		 #x20 #x00 #x00 #x39 #x02 #x00 #x06 #x25 #x25
		 #x19 #x02 #x16 #x09 #x11 #x19 #x16 #x11 #x13
		 #x18 #x08 #x10 #x18 #x00 #x01 #x1F #x06 #x12
		 #x1A #x12 #x2E #x19 #x02 #x00 #x33 #x38 #x00
		 #x0D #x06 #x3B #x13 #x0A #x02 #x00 #x27 #x00
		 #x00 #x00 #x33 #x13 #x3C #x00 #x11 #x14 #x04
		 #x11 #x1D #x34 #x13 #x01 #x00 #x14 #x27 #x00
		 #x2C #x10 #x21 #x2C #x02 #x33 #x00 #x00 #x00
		 #x37 #x12 #x2A #x31 #x02 #x00 #x12 #x06 #x09
		 #x37 #x12 #x2A #x31 #x14 #x0C #x00 #x00 #x00
		 #x39 #x0D #x12 #x10 #x0F #x00 #x00 #x27 #x03
		 #x37 #x12 #x0C #x31 #x05 #x00 #x31 #x00 #x00
		 #x37 #x20 #x0A #x31 #x00 #x00 #x00 #x00 #x00
		 #x11 #x13 #x0E #x01 #x0D #x11 #x05 #x25 #x24
		 #x0A #x24 #x0C #x08 #x0D #x21 #x00 #x00 #x00
		 #x37 #x06 #x3A #x31 #x05 #x02 #x0A #x1D #x16
		 #x38 #x14 #x0C #x00 #x08 #x06 #x20 #x1B #x34
		 #x0E #x02 #x06 #x00 #x02 #x1F #x19 #x20 #x08
		 #x37 #x10 #x21 #x31 #x12 #x0C #x00 #x00 #x00
		 #x01 #x2D #x30 #x01 #x2D #x00 #x01 #x2D #x30
		 #x33 #x34 #x06 #x01 #x18 #x00 #x01 #x18 #x08
		 #x31 #x20 #x34 #x31 #x20 #x05 #x31 #x20 #x08
		 #x1F #x3A #x20 #x14 #x3A #x20 #x0C #x00 #x20
		 #x0A #x20 #x06 #x30 #x1F #x0C #x00 #x20 #x00
		 #x35 #x20 #x05 #x34 #x14 #x09 #x30 #x20 #x11
		 #x08 #x18 #x18 #x08 #x18 #x18 #x08 #x33 #x20
		 #x04 #x16 #x06 #x36 #x06 #x0C #x01 #x03 #x00
		 #x2F #x08 #x18 #x1C #x00 #x18 #x00 #x20 #x18
		 #x00 #x18 #x14 #x35 #x1D #x06 #x14 #x00 #x3B
		 #x06 #x20 #x05 #x34 #x14 #x09 #x19 #x00 #x21
		 #x05 #x3A #x3A #x06 #x3A #x3A #x05 #x3A #x3A
		 #x01 #x23 #x00 #x01 #x23 #x00 #x01 #x23 #x08
		 #x01 #x32 #x02 #x01 #x32 #x02 #x01 #x32 #x02
		 #x15 #x04 #x03 #x15 #x17 #x03 #x15 #x17 #x03
		 #x07 #x2B #x03 #x07 #x17 #x03 #x07 #x17 #x03
		 #x04 #x1E #x06 #x1E #x3F #x0E #x09 #x11 #x13
		 #x0F #x29 #x05 #x09 #x28 #x09 #x09 #x09 #x01
		 #x08 #x0B #x0B #x1B #x0B #x0B #x1E #x0B #x00
		 #x08 #x0B #x0B #x0E #x0B #x0B #x1A #x0B #x00
		 #x11 #x1D #x06 #x08 #x10 #x04 #x02 #x06 #x2F
		 #x1F #x1C #x2F #x00 #x1C #x1C #x09 #x18 #x11
		 #x0B #x0C #x0C #x0B #x02 #x30 #x00 #x00 #x00
		 #x25 #x1C #x04 #x01 #x1C #x1D #x1D #x06 #x08
		 #x01 #x30 #x21 #x3F #x2E #x11 #x19 #x25 #x01
		 #x16 #x00 #x00 #x03 #x0C #x0A #x19 #x0A #x19
		 #x0E #x16 #x1B #x11 #x1D #x10 #x3C #x3A #x05
		 #x20 #x08 #x10 #x06 #x22 #x19 #x02 #x22 #x18
		 #x06 #x0C #x01 #x10 #x00 #x00 #x00 #x11 #x13
		 #x0A #x2B #x03 #x0A #x17 #x03 #x0A #x17 #x03
		 #x12 #x14 #x06 #x12 #x02 #x00 #x0A #x02 #x00
		 #x0A #x24 #x0C #x00 #x0A #x21 #x06 #x20 #x18
		 #x0A #x21 #x21 #x35 #x02 #x08 #x10 #x02 #x05
		 #x00 #x12 #x0F #x11 #x24 #x21 #x35 #x02 #x05
		 #x06 #x25 #x0C #x06 #x02 #x12 #x14 #x02 #x18
		 #x12 #x20 #x14 #x00 #x00 #x21 #x18 #x12 #x0B
		 #x0A #x24 #x06 #x00 #x20 #x08 #x25 #x02 #x00
		 #x24 #x02 #x35 #x18 #x12 #x14 #x34 #x00 #x18
		 #x12 #x14 #x0C #x00 #x0A #x21 #x35 #x02 #x00
		 #x00 #x26 #x03 #x06 #x27 #x03 #x06 #x27 #x03
		 #x26 #x03 #x00 #x27 #x03 #x00 #x27 #x03 #x00
		 #x11 #x04 #x03 #x00 #x36 #x03 #x00 #x36 #x03
		 #x06 #x04 #x03 #x07 #x17 #x03 #x07 #x17 #x03
		 #x0A #x20 #x24 #x25 #x03 #x06 #x08 #x02 #x0B
		 #x12 #x04 #x16 #x0A #x17 #x03 #x0A #x17 #x03
		 #x07 #x2B #x00 #x07 #x17 #x00 #x07 #x17 #x25
		 #x00 #x07 #x2B #x03 #x07 #x17 #x03 #x07 #x17
		 #x03 #x36 #x03 #x11 #x24 #x1D #x24 #x03 #x06
		 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
		 #x00 #x15 #x04 #x03 #x15 #x17 #x03 #x15 #x17
		 #x03 #x12 #x1D #x1D #x14 #x06 #x12 #x06 #x00
		 #x1C #x1C #x00 #x1C #x2F #x00 #x06 #x20 #x20
		 #x00 #x0B #x02 #x00 #x0B #x02 #x00 #x36 #x00
		 #x01 #x18 #x18 #x01 #x18 #x18 #x01 #x18 #x18
		 #x00 #x00 #x00 #x33 #x00 #x08 #x18 #x04 #x28
		 #x1F #x0C #x08 #x25 #x06 #x0E #x06 #x00 #x18
		 #x06 #x0E #x00 #x16 #x16 #x00 #x1D #x20 #x20
		 #x18 #x07 #x06 #x35 #x10 #x34 #x05 #x09 #x24
		 #x05 #x09 #x09 #x09 #x09 #x01 #x0D #x10 #x09
		 #x08 #x25 #x33 #x2E #x06 #x1B #x06 #x00 #x13
		 #x1C #x00 #x00 #x1C #x00 #x00 #x1C #x00 #x38
		 #x1D #x3D #x05 #x1D #x3D #x05 #x1D #x3D #x05
		 #x1D #x20 #x1D #x00 #x18 #x00 #x33 #x34 #x06
		 #x39 #x3C #x21 #x01 #x3C #x01 #x06 #x1F #x19
		 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x08
		 #x0A #x17 #x03 #x11 #x13 #x14 #x00 #x05 #x35
		 #x00 #x05 #x34 #x00 #x00 #x34 #x00 #x00 #x34
		 #x38 #x04 #x02 #x33 #x00 #x11 #x04 #x00 #x00
		 #x0C #x00 #x26 #x33 #x09 #x09 #x20 #x08 #x18
		 #x0F #x36 #x00 #x00 #x00 #x00 #x00 #x27 #x05
		 #x30 #x09 #x20 #x20 #x06 #x20 #x21 #x00 #x00
		 #x0A #x3A #x10 #x2B #x18 #x38 #x38 #x0E #x02
		 #x16 #x0C #x35 #x05 #x00 #x00 #x19 #x30 #x00
		 #x08 #x1C #x18 #x00 #x1C #x00 #x00 #x05 #x3A
		 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x38 #x20
		 #x1F #x0C #x08 #x25 #x06 #x08 #x00 #x00 #x34
		 #x06 #x1C #x05 #x25 #x1C #x25 #x1F #x18 #x34
		 #x33 #x20 #x26 #x0B #x02 #x00 #x34 #x09 #x09
		 #x00 #x06 #x36 #x00 #x00 #x11 #x24 #x0B #x34
		 #x20 #x20 #x00 #x00 #x39 #x02 #x08 #x1D #x00
		 #x00 #x08 #x00 #x3E #x00 #x00 #x37 #x08 #x1D
		 #x00 #x00 #x20 #x00 #x35 #x20 #x05 #x34 #x34
		 #x12 #x14 #x24 #x34 #x2E #x30 #x1F #x06 #x08
		 #x01 #x05 #x30 #x04 #x30 #x2E #x06 #x0E #x00
		 #x36 #x00 #x00 #x00 #x00 #x00 #x34 #x34 #x00
		 #x0A #x06 #x1B #x1F #x00 #x00 #x25 #x00 #x3B
		 #x25 #x10 #x06 #x00 #x00 #x0A #x10 #x07 #x03
		 #x0A #x10 #x01 #x00 #x00 #x00 #x16 #x19 #x35
		 #x06 #x12 #x10 #x19 #x10 #x00 #x00 #x00 #x3A
		 #x11 #x06 #x09 #x35 #x16 #x10 #x3E #x13 #x0D
		 #x24 #x3D #x10 #x0E #x12 #x33 #x03 #x06 #x30
		 #x00 #x26 #x00 #x00 #x27 #x00 #x00 #x3B #x08
		 #x06 #x0C #x0C #x20 #x0A #x06 #x11 #x14 #x00
		 #x18 #x24 #x06 #x0A #x10 #x18 #x11 #x24 #x18
		 #x10 #x25 #x05 #x06 #x3C #x05 #x06 #x00 #x00
		 #x06 #x0C #x0C #x00 #x00 #x12 #x24 #x1D #x1D))
   
   (make-array #x44 :element-type 'fixnum :initial-contents 
	       '(#x0000000 #x0800001 #x0040020 #x1440090
		 #x0A00081 #x1000000 #x1400020 #x0800008
		 #x0A03180 #x1002200 #x0800400 #x1418001
		 #x0080020 #x0841020 #x0203100 #x0203088
		 #x0A00820 #x0800120 #x08001C0 #x0810081
		 #x0A00089 #x0800401 #x0A010A0 #x0A01081
		 #x0818001 #x1A00220 #x0201100 #x0203420
		 #x0008000 #x0801020 #x0201420 #x0801190
		 #x0040000 #x0080820 #x0800002 #x0140002
		 #x0800100 #x0A03C20 #x0A00808 #x0A01008
		 #x0200540 #x0601209 #x0083100 #x0A03081
		 #x8800004 #x0058001 #x1001280 #x1008001
		 #x1200209 #x4018001 #x0040002 #x1000001
		 #x0010200 #x0800840 #x0A01181 #x4018801
		 #x0A10181 #x0800801 #x0040001 #x0011190
		 #x0858001 #x0040020 #x3200209 #x08000C0
		 #x4000020 #x0600081 #x1000000 #x1000180))
   3))

(defun make-ik1306 ()
  (declare (optimize (speed 3) (safety 0)(debug 0)))
  (ik13
   (make-array #x100 :element-type 'fixnum :initial-contents 
	       '#(#x0070000 #x0060040 #x0076A2F #x00B4C00
		  #x0090000 #x00B4D00 #x0090000 #x0055300
		  #x0090000 #x00B5400 #x0090000 #x0054600
		  #x0061000 #x00B4800 #x0065657 #x0057300
		  #x0090000 #x0075655 #x0071700 #x0060040
		  #x0070000 #x0070000 #x0074444 #x00C4545
		  #x0280058 #x0682825 #x08A0000 #x0280059
		  #x0800058 #x0800059 #x04D5F5F #x0FB2F22
		  #x0FB2F21 #x0F80000 #x0FB2F20 #x0940000
		  #x0B80059 #x0B80058 #x0830000 #x03D4343
		  #x0075E5E #x0075B00 #x0695900 #x007002B
		  #x0070028 #x0070003 #x0070028 #x0070052
		  #x0070015 #x00C0037 #x00F5C00 #x0075C01
		  #x0075D5D #x007285F #x0DC585B #x00C005C
		  #x0680000 #x0070A0A #x0075B59 #x0070254
		  #x02A5F5F #x0075F5F #x00B0076 #x0077700
		  #x00B0039 #x0063A2A #x01B3B2A #x0682828
		  #x0680000 #x0F05800 #x00B003D #x04A0000
		  #x0053200 #x0502800 #x0054E00 #x0560000
		  #x0530000 #x00B0076 #x0077700 #x03E5F5F
		  #x0DC0058 #x0050032 #x0682828 #x005002A
		  #x0682C2C #x0682828 #x0050039 #x0682828
		  #x0682C2C #x0CA0025 #x0070013 #x0070066
		  #x0070014 #x0070066 #x0070014 #x0F6005F
		  #x00B3E00 #x0065300 #x00B4E00 #x0065300
		  #x0063B58 #x0052A00 #x0070058 #x0184343
		  #x0FC7576 #x00A2828 #x0052A00 #x0065300
		  #x00C0000 #x0180000 #x0682F2F #x0053C00
		  #x0065300 #x00C0000 #x0182F2F #x0680000
		  #x007042E #x0051600 #x07A0000 #x0070447
		  #x00B164B #x0770000 #x00C3119 #x0180000
		  #x007005D #x0DC585F #x0830000 #x0680000
		  #x0695E5E #x0830000 #x0680000 #x00A0009
		  #x00B0016 #x00B0061 #x0185A5A #x0075866
		  #x0F00900 #x0840004 #x0052F26 #x068002F
		  #x0680027 #x0056D00 #x0180000 #x0920000
		  #x0F00959 #x0180000 #x00A0000 #x0B50015
		  #x0070011 #x0070052 #x0070066 #x0070001
		  #x0070001 #x0070066 #x0070001 #x0070066
		  #x0070001 #x0070001 #x0070066 #x0070001
		  #x0070066 #x0070002 #x0070066 #x0070001
		  #x0075D5D #x0070052 #x0075D5D #x0075D5D
		  #x0590003 #x00A5A00 #x00B2A00 #x01C7400
		  #x00B3F00 #x0185E00 #x00B7458 #x0B2005F
		  #x0F00947 #x0AE0000 #x00B5E63 #x0090000
		  #x0186B2C #x00C006E #x0180000 #x0180001
		  #x0072828 #x00B3000 #x0680000 #x00C3636
		  #x0C10000 #x0F07259 #x0A90000 #x0C45F00
		  #x0073131 #x0A95A5A #x0C45A5A #x0680000
		  #x00A0000 #x0690059 #x0CA2C00 #x0DC5931
		  #x0DC596B #x08D0000 #x00A5A5A #x007000E
		  #x0072E2E #x0074242 #x0073334 #x00B6265
		  #x0DB5E5E #x0070064 #x007075F #x0075F51
		  #x00B1A03 #x00F0051 #x0D40068 #x0075F5F
		  #x0070052 #x0070065 #x0CF0038 #x0180067
		  #x00A4242 #x005004E #x0070051 #x0066000
		  #x0065300 #x005004F #x0065300 #x0064650
		  #x005004F #x0070050 #x0070059 #x0070052
		  #x01B353E #x005002A #x0070058 #x007000E
		  #x0063B51 #x005004E #x0075800 #x0184343
		  #x00A4242 #x0066000 #x0063B00 #x0070000
		  #x0075000 #x0605259 #x0837125 #x0680000
		  #x0070023 #x0070024 #x0072F29 #x0070041
		  #x1060040 #x0074900 #x0075F5F #x0094A4A))

   (make-array #x480 :element-type 'fixnum :initial-contents 
	       '(#x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
		 #x2C #x2A #x27 #x13 #x2B #x27 #x13 #x2B #x27
		 #x34 #x2A #x27 #x13 #x2B #x27 #x13 #x2B #x27
		 #x29 #x2A #x35 #x29 #x2B #x35 #x29 #x2B #x35
		 #x29 #x12 #x35 #x29 #x3F #x35 #x29 #x3F #x35
		 #x2E #x00 #x00 #x2D #x02 #x00 #x00 #x00 #x00
		 #x2A #x02 #x00 #x2D #x02 #x00 #x00 #x00 #x00
		 #x03 #x12 #x05 #x2D #x02 #x00 #x00 #x00 #x00
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x0E #x02 #x00 #x24 #x02 #x00 #x24 #x02 #x00
		 #x30 #x1D #x05 #x2F #x1D #x00 #x00 #x1D #x00
		 #x22 #x00 #x00 #x2D #x02 #x00 #x00 #x00 #x00
		 #x0C #x00 #x00 #x2D #x02 #x00 #x00 #x00 #x00
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x00 #x00 #x00 #x00 #x00 #x00 #x24 #x25 #x00
		 #x31 #x00 #x00 #x2D #x02 #x00 #x00 #x00 #x00
		 #x0E #x0F #x0F #x00 #x00 #x00 #x0F #x0F #x0F
		 #x0E #x34 #x05 #x00 #x00 #x00 #x00 #x00 #x00
		 #x18 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x00
		 #x00 #x00 #x00 #x00 #x03 #x18 #x00 #x00 #x00
		 #x18 #x25 #x00 #x03 #x18 #x00 #x00 #x00 #x00
		 #x1B #x03 #x39 #x00 #x00 #x00 #x14 #x18 #x00
		 #x36 #x00 #x00 #x03 #x0B #x00 #x00 #x00 #x00
		 #x00 #x00 #x00 #x03 #x18 #x00 #x00 #x00 #x00
		 #x37 #x1E #x00 #x00 #x1E #x00 #x00 #x00 #x00
		 #x01 #x06 #x07 #x01 #x06 #x07 #x01 #x06 #x07
		 #x34 #x12 #x00 #x00 #x00 #x00 #x00 #x00 #x00
		 #x3C #x00 #x00 #x2D #x02 #x00 #x00 #x00 #x00
		 #x3E #x00 #x00 #x2D #x02 #x00 #x00 #x00 #x00
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x01 #x25 #x00 #x01 #x25 #x00 #x24 #x02 #x00
		 #x03 #x04 #x02 #x03 #x04 #x02 #x24 #x02 #x00
		 #x01 #x06 #x07 #x01 #x06 #x07 #x24 #x02 #x00
		 #x01 #x04 #x08 #x01 #x04 #x08 #x24 #x00 #x1A
		 #x03 #x06 #x09 #x03 #x06 #x09 #x24 #x00 #x02
		 #x03 #x25 #x00 #x03 #x25 #x00 #x24 #x25 #x00
		 #x03 #x00 #x38 #x03 #x00 #x0B #x03 #x25 #x00
		 #x24 #x25 #x00 #x24 #x25 #x0E #x05 #x00 #x00
		 #x03 #x25 #x00 #x03 #x25 #x00 #x03 #x25 #x00
		 #x00 #x00 #x19 #x05 #x00 #x19 #x05 #x00 #x00
		 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x29 #x12
		 #x00 #x21 #x00 #x00 #x21 #x24 #x25 #x03 #x25
		 #x00 #x0D #x02 #x00 #x0D #x02 #x00 #x0D #x02
		 #x00 #x17 #x00 #x00 #x17 #x24 #x05 #x00 #x00
		 #x24 #x00 #x05 #x24 #x00 #x05 #x24 #x00 #x05
		 #x24 #x25 #x00 #x24 #x25 #x00 #x24 #x25 #x00
		 #x13 #x0A #x00 #x00 #x03 #x0B #x00 #x28 #x00
		 #x00 #x03 #x05 #x00 #x03 #x05 #x00 #x03 #x05
		 #x1B #x03 #x00 #x0B #x03 #x0B #x00 #x00 #x00
		 #x2C #x02 #x00 #x24 #x02 #x00 #x24 #x02 #x00
		 #x0E #x0F #x0F #x00 #x00 #x00 #x02 #x00 #x00
		 #x0E #x0F #x0F #x00 #x00 #x00 #x0F #x0F #x00
		 #x00 #x16 #x00 #x00 #x16 #x00 #x00 #x16 #x00
		 #x00 #x17 #x00 #x00 #x17 #x00 #x00 #x00 #x21
		 #x00 #x00 #x17 #x00 #x00 #x17 #x24 #x02 #x00
		 #x00 #x00 #x00 #x00 #x00 #x00 #x29 #x12 #x00
		 #x14 #x0F #x0F #x00 #x00 #x00 #x0F #x00 #x00
		 #x24 #x0F #x0F #x00 #x00 #x00 #x0F #x00 #x00
		 #x13 #x0A #x00 #x00 #x03 #x0B #x00 #x00 #x00
		 #x1B #x18 #x0B #x00 #x00 #x00 #x00 #x00 #x00
		 #x13 #x0F #x0F #x00 #x00 #x00 #x00 #x2A #x00
		 #x1B #x03 #x3D #x00 #x00 #x00 #x00 #x00 #x00
		 #x00 #x3B #x00 #x00 #x3B #x00 #x12 #x14 #x00
		 #x0E #x00 #x00 #x0F #x00 #x00 #x05 #x24 #x02
		 #x03 #x00 #x25 #x03 #x00 #x25 #x03 #x00 #x25
		 #x00 #x00 #x20 #x00 #x00 #x20 #x00 #x00 #x20
		 #x00 #x11 #x05 #x00 #x11 #x05 #x00 #x11 #x05
		 #x00 #x11 #x25 #x00 #x11 #x25 #x00 #x11 #x25
		 #x0E #x0F #x0F #x00 #x00 #x00 #x10 #x00 #x00
		 #x03 #x00 #x00 #x33 #x00 #x00 #x00 #x00 #x00
		 #x0E #x0F #x0F #x2A #x0F #x0F #x12 #x00 #x00
		 #x00 #x00 #x00 #x1B #x1C #x00 #x00 #x00 #x00
		 #x03 #x15 #x00 #x03 #x15 #x00 #x03 #x15 #x00
		 #x1B #x02 #x00 #x00 #x00 #x00 #x1B #x02 #x00
		 #x00 #x00 #x00 #x03 #x00 #x12 #x00 #x00 #x00
		 #x00 #x00 #x00 #x03 #x12 #x12 #x12 #x12 #x00
		 #x00 #x00 #x00 #x00 #x00 #x00 #x24 #x23 #x02
		 #x00 #x00 #x00 #x00 #x00 #x24 #x23 #x02 #x00
		 #x26 #x27 #x00 #x28 #x27 #x00 #x28 #x27 #x00
		 #x00 #x26 #x27 #x00 #x28 #x27 #x00 #x28 #x27
		 #x29 #x2A #x27 #x29 #x2B #x27 #x29 #x2B #x3A
		 #x0E #x12 #x12 #x12 #x00 #x00 #x10 #x00 #x00
		 #x0E #x12 #x00 #x00 #x00 #x00 #x10 #x00 #x00
		 #x0E #x0F #x0F #x00 #x00 #x0F #x0F #x0F #x00
		 #x00 #x00 #x00 #x25 #x00 #x0E #x0F #x0F #x0F
		 #x0E #x0F #x0F #x00 #x00 #x0F #x0F #x0F #x0F
		 #x00 #x00 #x00 #x00 #x00 #x00 #x0E #x18 #x00
		 #x24 #x18 #x00 #x00 #x00 #x00 #x00 #x00 #x00
		 #x00 #x1D #x00 #x00 #x1D #x00 #x00 #x1D #x00
		 #x1F #x1A #x00 #x00 #x00 #x00 #x00 #x00 #x00
		 #x00 #x16 #x18 #x00 #x00 #x00 #x00 #x00 #x00
		 #x00 #x16 #x05 #x00 #x16 #x05 #x00 #x16 #x05
		 #x00 #x16 #x02 #x00 #x16 #x02 #x00 #x16 #x02
		 #x03 #x21 #x02 #x03 #x21 #x02 #x03 #x21 #x02
		 #x18 #x0F #x00 #x00 #x00 #x00 #x00 #x00 #x00
		 #x00 #x00 #x00 #x1B #x03 #x0B #x00 #x00 #x00
		 #x03 #x12 #x00 #x00 #x00 #x00 #x00 #x00 #x00
		 #x00 #x00 #x00 #x1B #x03 #x3D #x00 #x00 #x00
		 #x24 #x18 #x03 #x18 #x05 #x03 #x18 #x05 #x00
		 #x03 #x00 #x32 #x03 #x00 #x32 #x03 #x00 #x32
		 #x24 #x33 #x00 #x00 #x33 #x00 #x00 #x00 #x00
		 #x00 #x00 #x21 #x00 #x00 #x21 #x00 #x00 #x00
		 #x00 #x2C #x2A #x27 #x13 #x2B #x27 #x00 #x00
		 #x03 #x25 #x00 #x03 #x25 #x00 #x13 #x09 #x00
		 #x00 #x3B #x05 #x00 #x3B #x05 #x00 #x3B #x05
		 #x00 #x0D #x05 #x00 #x0D #x05 #x00 #x0D #x05
		 #x13 #x00 #x00 #x00 #x00 #x00 #x00 #x00 #x07
		 #x1B #x18 #x0B #x00 #x00 #x00 #x00 #x00 #x00
		 #x00 #x17 #x00 #x00 #x17 #x0E #x05 #x0D #x02
		 #x18 #x00 #x25 #x00 #x00 #x00 #x00 #x00 #x00
		 #x00 #x00 #x03 #x00 #x00 #x00 #x18 #x00 #x00
		 #x13 #x09 #x00 #x00 #x09 #x00 #x00 #x09 #x00
		 #x0E #x0F #x02 #x24 #x25 #x00 #x24 #x25 #x00
		 #x00 #x00 #x00 #x29 #x0F #x0F #x0F #x12 #x00
		 #x00 #x29 #x12 #x00 #x29 #x3F #x00 #x13 #x0F
		 #x00 #x3D #x00 #x00 #x3D #x00 #x00 #x3D #x00
		 #x1B #x03 #x00 #x0B #x03 #x0B #x13 #x39 #x24
		 #x0E #x02 #x00 #x24 #x02 #x00 #x13 #x07 #x00
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F
		 #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F #x3F))

   (make-array #x44 :element-type 'fixnum :initial-contents 
	       '(#x0000000 #x0800008 #x0040020 #x0800001
		 #x0800021 #x0080020 #x0A00028 #x0040100
		 #x4000100 #x0010100 #x0A00101 #x0201089
		 #x0213201 #x0800004 #x0800800 #x0800820
		 #x0200088 #x4810002 #x0A00820 #x0800400
		 #x0801000 #x0100000 #x8800004 #x0008000
		 #x1400020 #x0800005 #x4000020 #x0A00180
		 #x0100000 #x4000001 #x8241004 #x0400000
		 #x0080001 #x0040001 #x0212801 #x0200808
		 #x0800000 #x0010020 #x0A00808 #x0040090
		 #x0A01008 #x0800401 #x0A00081 #x0A01081
		 #x0803400 #x0A01001 #x0A11801 #x0011001
		 #x0A10801 #x0213801 #x0098001 #x0818001
		 #x0800420 #x0880090 #x0203C08 #x0200809
		 #x0A00089 #x0203090 #x0840090 #x0810002
		 #x0210801 #x0210081 #x0010000 #x0200090
		 #x0210081 #x0212801 #x0A01020 #x0A01020))
   6))

;; Emulator of IR2 ring memories
  
(defun make-ir2 (&key (set nil))
  (declare (optimize (speed 3) (safety 0)(debug 0)))
  (let ((mem (if set (make-array #xFC :element-type 'fixnum :initial-contents set)
		 (make-array #xFC :element-type 'fixnum)))
	(tact 0)
	(output 0))
    (declare (type fixnum tact output))
    (lambda (&key step reset dump out)
      (cond (step
	     (setf output (aref mem tact)
		   (aref mem tact) step
		   tact (mod (1+ tact) #xFC))
		   output)
	    (reset
	     (setf mem (make-array #xFC) tact 0))
	    (dump
	     (list mem tact))
	    (out (aref mem tact))))))

;; Emulator of arbitrary length ring memories


(defun make-irx (len &key (set nil))
  (declare (optimize (speed 3) (safety 0)(debug 0)))
  (let ((mem (if set (make-array len :element-type 'fixnum :initial-contents set)
		 (make-array len :element-type 'fixnum)))
	(tact 0)
	(output 0))
    (declare (type fixnum len tact output))
    (lambda (&key step reset dump out)
      (cond (step
	     (setf output (aref mem tact)
		   (aref mem tact) step
		   tact (mod (1+ tact) len))
		   output)
	    (reset
	     (setf mem (make-array len) tact 0))
	    (dump
	     (list mem tact))
	    (out (aref mem tact))))))
