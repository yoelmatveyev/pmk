(defsystem :pmk
  :name "Emulator of Soviet programmable calculators"
  :version "0.0.5"
  :maintainer "Yoel Matveyev"
  :author "Yoel Matveyev"
  :licence "GNU General Public License v3.0"
  :description "Emulator of Soviet programmable calculators and tools for exploring their features"
  :long-description "Emulator of Soviet programmable calculators (B3-34 and MK-61) and tools for exploring their features"
  :components ((:file "package")
  	       (:file "pmk")	     	     
	       (:file "chips")
	       (:file "machine")))

