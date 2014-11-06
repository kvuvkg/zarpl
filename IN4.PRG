*** 19.06.92
*** Работа с аpхивом индексация
*** in1.prg
set device to screen
*PARA a
priv mes,mes1,k,s,m,li,tn1,tn2,kk,vv,v_v,v_v1,v_v2,k,gt,gb,i,f_io,vv3,vv4,v_v3,v_v4,M_M
PRIV S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_BL,S_ZP,RAZN,s_b,S_IN
M_M='  '
sele f
use ssp
sele d
use ind
*use superlis index superpon
@ 0,0 CLEAR
STORE 0 TO S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_BL,S_ZP,RAZN,s_b,S_IN

STORE 0 TO K,s,m,li
li=1
Per='           '
Tnn='             '
store '  ' to mes,mes1
STORE '      ' TO TN1,TN2,gt,gb
store '    ' to kk
STORE '                                                    ' TO VV1,vv2,tt,f_io,vv3,vv4
store space(60) to vv,v_v,v_v1,v_v2,v_v3,v_v4
k=1
set cursor on
set color to &bcol
@ 1,1 to 22,70
@ 1,1 say 'Индексация з/пл по одному цеху по одному или гpуппе табельных номеpов'
set color to &stdc
@ 3,5 SAY 'Введите номер подразделения для котоpого сделать выбоpку'
@ 5,5 get kk picture '9999' 
*valid(kk=ssp->ksp)
@ 23,0 clear
@ 23,1 say ' -ввод F1-помощь {Esc}-выход '
@ 24,1 say ' '
read
if lastkey()=27
retu
endif
@ 5,20 say 'Подpазделение'
sele f
locate for ksp=kk
if found()
@ 5,34 say ssp->nbrig
endif
sele a
use base&kk
go top
gt=tn
go bottom
gb=tn
go top

@ 7,5 say 'Введите месяц за котоpый нужно пpовести индексацию'
@ 9,5 get per picture '99.99-99.99'
@ 11,5 SAY 'Введите табельный номеp или гpуппу табельных номеpов'
@ 12,5 say 'Имеющиеся табельные номеpа находятся в пpеделах ['+gt+'-'+gb+']'
@ 15,5 GET TNN PICTURE '999999-999999'
*@ 17,5 say 'Введите виды оплат'
*@ 18,2 GET VV1 PICTURE '*999*999*999*999*999*999*999*999*999*999*999*999*999'
*@ 19,2 get vv2 picture '*999*999*999*999*999*999*999*999*999*999*999*999*999'
*@ 20,2 get vv3 picture '*999*999*999*999*999*999*999*999*999*999*999*999*999'
*@ 21,2 get vv4 picture '*999*999*999*999*999*999*999*999*999*999*999*999*999'


  @ 23,0 clear
  @ 23,1 SAY '┘-ввод F1-помощь {Esc}-выход '
  @ 24,1 say ''
read

@ 23,0 CLEAR
@ 23,1 SAY 'Ж Д И Т Е!'
set cursor off

if lastkey()=27
retu
endif
set console on
*vv=vv1+vv2+vv3+vv4
*for i=2 to 50 step 4
*if substr(vv1,i,3)#' '
*v_v1=v_v1+SUBSTR(vv1,I,3)+' '
*endif
*next
*for i=2 to 50 step 4
*if substr(vv2,i,3)#' '
*V_V2=V_V2+SUBSTR(VV2,I,3)+' '
*ENDIF
*NEXT
*for i=2 to 50 step 4
*if substr(vv3,i,3)#' '
*v_v3=v_v3+SUBSTR(vv3,I,3)+' '
*endif
*next
*for i=2 to 50 step 4
*if substr(vv4,i,3)#' '
*v_v4=v_v4+SUBSTR(vv4,I,3)+' '
*endif
*next

TN1=SUBSTR(TNN,1,6)
TN2=SUBSTR(TNN,8,6)
IF TN2=' '
TN2=TN1
ENDIF

mes=val(substr(per,1,2))  && месяц индексации
mes1=val(substr(per,7,2))

sele a
*use base&kk
set filter to tn>=tn1.and.tn<=tn2
GO TOP

do while.not.eof()
i=recno()

for i=1 to 12
nu=if(i<10,str(i,1),str(i,2))

IF MES<10
M_M='0'+LTRIM(STR(MES))
ELSE
M_M='1'+LTRIM(STR(MES))
ENDIF

if kmn&nu=M_M

if vo&nu='216'.OR.VO&NU='217'.OR.VO&NU='218'
S_B=S_B+SVON&NU       && СУММА БОЛЬНИЧНЫХ
ENDIF

IF VO&NU>='520'.and.VO&NU<='526'

IF VO&NU='520'
 S_1=S_1+SVON&NU
ENDIF
IF VO&NU='521'
 S_2=S_2+SVON&NU
ENDIF
IF VO&NU='522'
 S_3=S_3+SVON&NU
ENDIF
IF VO&NU='523'
 S_4=S_4+SVON&NU
ENDIF
IF VO&NU='524'
 S_5=S_5+SVON&NU
ENDIF
IF VO&NU='525'
 S_6=S_6+SVON&NU
ENDIF
IF VO&NU='526'
 S_7=S_7+SVON&NU
ENDIF
ENDIF
s=s+svon&nu
endif
next

*S=S-S_1-S_2-S_3-S_4-S_5-S_6-S_7
*S_IN=S
*SKIP
*MES=MES+1
*i1=i1+1
*ENDDO
*MES=VAL(SUBSTR(PER,1,2))

sele b
use seba&kk
*go (i-1)*12+mes  && если необходимо с опеделенного месяца а не с начала базы
go (i-1)*12+1
K=13
i1=1
DO WHILE K-i1#0
*DO WHILE K-mes#0

for i=1 to 12
nu=if(i<10,str(i,1),str(i,2))

IF MES<10
M_M='0'+LTRIM(STR(MES))
ELSE
M_M='1'+LTRIM(STR(MES))
ENDIF

if kmn&nu=M_M
*.and.'*'+VO&NU$VV
*.OR.(KMN&NU<=STR(MES1).AND.'*'+VO&NU$VV)

if vo&nu='216'.OR.VO&NU='217'.OR.VO&NU='218'
S_B=S_B+SVON&NU       && СУММА БОЛЬНИЧНЫХ
ENDIF

IF VO&NU>='520'.and.VO&NU<='526'

IF VO&NU='520'
 S_1=S_1+SVON&NU
ENDIF
IF VO&NU='521'
 S_2=S_2+SVON&NU
ENDIF
IF VO&NU='522'
 S_3=S_3+SVON&NU
ENDIF
IF VO&NU='523'
 S_4=S_4+SVON&NU
ENDIF
IF VO&NU='524'
 S_5=S_5+SVON&NU
ENDIF
IF VO&NU='525'
 S_6=S_6+SVON&NU
ENDIF
IF VO&NU='526'
 S_7=S_7+SVON&NU
ENDIF
ENDIF
s=s+svon&nu
endif
next

S=S-S_1-S_2-S_3-S_4-S_5-S_6-S_7
S_IN=S
SKIP
*MES=MES+1
i1=i1+1
ENDDO
MES=VAL(SUBSTR(PER,1,2))


sele d
for i=1 to 3
nu=str(i,1)

if s#0
do case
case s<=su1
s=s*pr1*pro/10000
case s>su1.and.s<=su2
s=((s-su1)*pr2*pro/10000)+(su1*pro*pr1/10000)
case s>su3
s=pr3
endcase
exit
endif
next

IF S#0
IF S_B#0
RAZN=S_B*100/S_IN
S_BL=RAZN*S
S_ZP=(100-RAZN)*S
ENDIF
ENDIF
sele a



IF S_B#0
FOR I=1 TO 12
NU=IF(I<10,STR(I,1),STR(I,2))
IF VO&NU='   '
REPLACE VO&NU WITH '601',KMN&NU WITH IF(MES<10,'0'+LTRIM(str(MES)),'1'+LTRIM(STR(MES))),SVON&NU WITH S_BL/100
EXIT
ENDIF
NEXT

FOR I=1 TO 12
NU=IF(I<10,STR(I,1),STR(I,2))
IF VO&NU='   '
REPLACE VO&NU WITH '600',KMN&NU WITH IF(MES<10,'0'+LTRIM(STR(MES)),'1'+LTRIM(STR(MES))),SVON&NU WITH S_zp/100
EXIT
ENDIF
NEXT

else


for i=1 to 12
nu=if(i<10,str(i,1),str(i,2))
if vo&nu='   '
replace vo&nu with '600',kmn&nu  WITH IF(MES<10,'0'+LTRIM(STR(MES)),'1'+LTRIM(STR(MES))),svon&nu with s
exit
endif
next
ENDIF




IF S_1#0
FOR I=1 TO 12
NU=IF(I<10,STR(I,1),STR(I,2))
IF VO&NU='   '
REPLACE VO&NU WITH '620',KMN&NU  WITH IF(MES<10,'0'+LTRIM(STR(MES)),'1'+LTRIM(STR(MES))),SVON&NU WITH S_1*(IND->PRO)/100
EXIT
ENDIF
NEXT
ENDIF

IF S_2#0
FOR I=1 TO 12
NU=IF(I<10,STR(I,1),STR(I,2))
IF VO&NU='   '
REPLACE VO&NU WITH '621',KMN&NU  WITH IF(MES<10,'0'+LTRIM(STR(MES)),'1'+LTRIM(STR(MES))),SVON&NU WITH S_2*(IND->PRO)/100
EXIT
ENDIF
NEXT
ENDIF

IF S_3#0
FOR I=1 TO 12
NU=IF(I<10,STR(I,1),STR(I,2))
IF VO&NU='   '
REPLACE VO&NU WITH '622',KMN&NU WITH IF(MES<10,'0'+LTRIM(STR(MES)),'1'+LTRIM(STR(MES))),SVON&NU WITH S_3*(IND->PRO)/100
EXIT
ENDIF
NEXT
ENDIF

IF S_6#0
FOR I=1 TO 12
NU=IF(I<10,STR(I,1),STR(I,2))
IF VO&NU='   '
REPLACE VO&NU WITH '625',KMN&NU WITH IF(MES<10,'0'+LTRIM(STR(MES)),'1'+LTRIM(STR(MES))),SVON&NU WITH S_6*(IND->PRO)/100
EXIT
ENDIF
NEXT
ENDIF

IF S_7#0
FOR I=1 TO 12
NU=IF(I<10,STR(I,1),STR(I,2))
IF VO&NU='   '
REPLACE VO&NU WITH '626',KMN&NU WITH IF(MES<10,'0'+LTRIM(STR(MES)),'1'+LTRIM(STR(MES))),SVON&NU WITH S_7*(IND->PRO)/100
EXIT
ENDIF
NEXT
ENDIF


STORE 0 TO S,S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_BL,RAZN,S_ZP,S_IN,s_b
Skip
enddo
close all
clear
set device to screen
set print off
retu

