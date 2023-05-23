# pmk
Emulator or Soviet programmable calculators and related virtual devices

This package emulates the calculators MK-61, B3-34 and virtual ones based on the same microcontrollers. Pressing keys is emulated by entering their internal coordinates. Run more steps, if your key input doesn't work.

There is no graphic interface yet. Here is an example of a session hacking undocumented features of the calculator:

CL-USER\> (asdf:load-system "pmk")\n
T
CL-USER\> (defparameter mk61 (cl-pmk:make-calc))\n
MK61
CL-USER> (funcall mk61 :verbon t) ; Turn on verbose output of all display changes
T
CL-USER> (funcall mk61 :press '(5 8)) (funcall mk61 :steps 5) ; Delete 0/0, wait for the error
| 0          |
| EГГ0Г      |

| EГГ0Г      |
281474671632383
0
CL-USER> (funcall mk61 :press '(9 8)) ; ВП
| ГГГ0Г    00|
| Г,ГГ0Г    00|
0
CL-USER> (funcall mk61 :steps 1)(funcall mk61 :press '(11 8)) ; В^
|  ,          |
0
CL-USER> (funcall mk61 :steps 1) (funcall mk61 :press '(10 8)) ; Cx
| 0,          |
0
CL-USER> (funcall mk61 :press '(9 8)) ; ВП
| 1,        00|
0
CL-USER> (funcall mk61 :steps 1) (funcall mk61 :press '(7 1)) ; 5
| 1        00|
| 1        05|
| 1,        05|
0
CL-USER> (funcall mk61 :steps 1) (funcall mk61 :press '(2 1)) ; 0
| 1        05|
| 1        50|
| 1,        50|
0
CL-USER> (funcall mk61 :press '(11 9)) ; F
0
CL-USER> (funcall mk61 :press '(4 8)) ; X^2
| 1        50|
| EГГ0Г      |
0
CL-USER> (funcall mk61 :press '(11 9)) ; F
0
CL-USER> (funcall mk61 :press '(4 8)) ; X^2
| 3ГГ0Г      |
0
