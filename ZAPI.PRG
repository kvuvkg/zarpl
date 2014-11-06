para kp
clear
@ 5,10 say '†„ˆ’…!ˆ„…’ ‡€ˆ‘œ ˆ”Œ€–ˆˆ!'
kk='    '
tn1='      '
su1=0
vo_1='   '
mes1='  '
sele a
use po_&kp
do while.not.eof()
kk=podr
tn1=tn
vo_1=vo
do case
case &kp=1
su1=su3+su36+su610
case &kp=2
su1=su3+su36+su610+su113+su138 +su18
case &kp=3
su1=su3+su36
case &kp=4.or.&kp=5.or.&kp=6
su1=su3
endcase
mes1=mes
if su1#0
sele b
use base&kk
locate for tn=tn1
if found()
for vnu=1 to 12
nu=ltrim(str(vnu))
if vo&nu='   '
replace vo&nu with vo_1;
        svon&nu with su1*100;
        KMN&NU WITH  MEs1
exit
endif
next
endif
endif
sele a
su1=0
skip
if eof()
exit
endif
enddo
return
