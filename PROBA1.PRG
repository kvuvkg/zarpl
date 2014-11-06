PARA Q,W,E,R,T
clear
private s1,s2,s3,s4,rs,koe,z,z1
STORE 0 TO S1,S2,S3,S4,RS,OKS1,S,a,koe,z1,z
set cursor on
CEX1='    '
CLEAR
SET CONFIRM ON
@ 5,5 SAY '‚‚…„ˆ’… ”Ž„ ‡/‹'
@ 7,5 GET RS PICTURE '9999999999.99'
READ
SET CONFIRM OFF
SET COLOR TO &ECOL
@ 24,10 say " † „ ˆ ’ … !"
a=2
SELE 1
USE SKL
CEX1=CEX
DO WHILE.NOT.EOF()
SELE 0
USE BASE&CEX1
LOCATE FOR TN=SKL->TN
OKS1=OKS
use
SELE 1
S=CH*OKS1/10000  && ‘“ŒŒ€ ‡/‹
REPLACE SZP WITH S,FOND WITH RS
s2=s2+(szp*kty)
S=0 
SKIP
cex1=cex
ENDDO
SELE 1
GO TOP
SUM SZP TO S1
koe=(RS-s1)/S2
GO TOP
cex1=cex
DO WHILE.NOT.EOF()
S3=koe*SZP*KTY  && ˆ€Ž’ŽŠ
DO CASE
CASE PROC#0.AND.DEPROC=0
S4=(S3+SZP)*PROC*0.01            &&  …ŒˆŸ
case proc=0.and.deproc#0
s4=(s3+szp)*(1-deproc*0.01)
CASE PROC#0.AND.DEPROC#0
S4=(S3+SZP)*(PROC*0.01)-(s3+szp)*(deproc*0.01)
ENDCASE
REPLACE PR WITH S3,SPR WITH S4
sele 0
use base&cex1
locate for tn=skl->tn 
if found()
for vnu=1 to 12
nu=ltrim(str(vnu))
if vo&nu='   '.and. s3#0.or.vo&nu='399'.and.s3#0
replace vo&nu with '399';
        svon&nu with skl->pr*100;
        KMN&NU WITH  E
exit
endif
next
endif
s3=0
s4=0
use
sele 1
SKIP
cex1=cex
ENDDO
set cursor off
close all
return
