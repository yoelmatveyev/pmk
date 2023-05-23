# pmk
Emulator or Soviet programmable calculators and related virtual devices

This package emulates the calculators MK-61, B3-34 and virtual ones based on the same microcontrollers. Pressing keys is emulated by entering their internal coordinates. Run more steps, if your key input doesn't work.

There is no graphic interface yet. Here is an example of a session hacking undocumented features of the calculator:

CL-USER> (asdf:load-system "pmk")<br />
T<br />
CL-USER> (defparameter mk61 (cl-pmk:make-calc))<br />
MK61<br />
CL-USER> (funcall mk61 :verbon t) ; Turn on verbose output of all display changes<br />
T<br />
CL-USER> (funcall mk61 :press '(5 8)) (funcall mk61 :steps 5) ; Delete 0/0, wait for the error<br />
 <pre>| 0          |<br />
| EГГ0Г      |<br />
281474671632383<br />
0<br />
CL-USER> (funcall mk61 :press '(9 8)) ; ВП<br />
| ГГГ0Г    00|<br />
| Г,ГГ0Г    00|<br />
0<br />
CL-USER> (funcall mk61 :steps 1)(funcall mk61 :press '(11 8)) ; В^<br />
|  ,          |<br />
0<br />
CL-USER> (funcall mk61 :steps 1) (funcall mk61 :press '(10 8)) ; Cx<br />
| 0,          |<br />
0<br />
CL-USER> (funcall mk61 :press '(9 8)) ; ВП<br />
| 1,        00|<br />
0<br />
CL-USER> (funcall mk61 :steps 1) (funcall mk61 :press '(7 1)) ; 5<br />
| 1        00|<br />
| 1        05|<br />
| 1,        05|<br />
0<br />
CL-USER> (funcall mk61 :steps 1) (funcall mk61 :press '(2 1)) ; 0<br />
| 1        05|<br />
| 1        50|<br />
| 1,        50|<br />
0<br />
CL-USER> (funcall mk61 :press '(11 9)) ; F<br />
0<br />
CL-USER> (funcall mk61 :press '(4 8)) ; X^2<br />
| 1        50|<br />
| EГГ0Г      |<br />
0<br />
CL-USER> (funcall mk61 :press '(11 9)) ; F<br />
0<br />
CL-USER> (funcall mk61 :press '(4 8)) ; X^2<br />
| 3ГГ0Г      |<br />
0<br /> </pre />
