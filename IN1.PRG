set device to screen
priv mes,mes1,k,s,m,li,tn1,tn2,kk,vv,v_v,v_v1,v_v2,k,gt,gb,i,f_io,vv3,vv4,v_v3,v_v4,M_M
PRIV S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_BL,S_ZP,RAZN,s_b,S_IN
M_M='  '
sele f
use ssp
sele d
use ind
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
set confirm on
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
TN1=SUBSTR(TNN,1,6)
TN2=SUBSTR(TNN,8,6)
IF TN2=' '
TN2=TN1
ENDIF
mes=val(substr(per,1,2))  && месяц индексации
mes1=val(substr(per,7,2))
sele a
use base&kk
set filter to tn>=tn1.and.tn<=tn2
GO TOP
if mes<10
m_m='0'+ltrim(str(mes))
else
m_m=ltrim(str(mes))
endif
do while.not.eof()
i=recno()
for i3=1 to 12
nu=if(i3<10,str(i3,1),str(i3,2))
if vo&nu='600'.or.vo&nu='601'.or.vo&nu='620'.or.vo&nu='621'.or.vo&nu='622'.or.vo&nu='625'.or.vo&nu='626'
replace vo&nu with '   ',svon&nu with 0,kmn&nu with '  '
endif
next
for i3=1 to 12
nu=if(i3<10,str(i3,1),str(i3,2))
if kmn&nu=M_M
if vo&nu='216'.OR.VO&NU='217'.OR.VO&NU='218'
S_B=S_B+SVON&NU       && СУММА БОЛЬНИЧНЫХ
ENDIF
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
s=s+svon&nu
endif
next
S=S-S_1-S_2-S_3-S_4-S_5-S_6-S_7
store 0 to s_1,s_2,s_3,s_4,s_5,s_6,s_7
sele b
use seba&kk
*go (i-1)*12+mes  && если необходимо с опеделенного месяца а не с начала базы
go (i-1)*12+1
K=13
i1=1
DO WHILE K-i1#0
*DO WHILE K-mes#0
for i3=1 to 12
nu=if(i3<10,str(i3,1),str(i3,2))
if kmn&nu=M_M
*.and.'*'+VO&NU$VV
*.OR.(KMN&NU<=STR(MES1).AND.'*'+VO&NU$VV)
if vo&nu='216'.OR.VO&NU='217'.OR.VO&NU='218'
S_B=S_B+SVON&NU       && СУММА БОЛЬНИЧНЫХ
ENDIF
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
s=s+svon&nu
endif
next
S=S-S_1-S_2-S_3-S_4-S_5-S_6-S_7
S_IN=S
SKIP
*MES=MES+1
i1=i1+1
ENDDO
sele d
use ind
For i3=1 to 3
nu=str(i3,1)
if s#0
do case
case s<=su1
s=s*pr1*pro/10000
case  s>su2.and.s<=su3
s=((s-su2)*pr2*pro/10000)+(su1*pro*pr1/10000)
case  s>su3
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
FOR I3=1 TO 12
NU=IF(I3<10,STR(I3,1),STR(I3,2))
IF VO&NU='   '
REPLACE VO&NU WITH '601',KMN&NU WITH m_m,SVON&NU WITH S_BL/100
EXIT
ENDIF
NEXT
FOR I3=1 TO 12
NU=IF(I3<10,STR(I3,1),STR(I3,2))
IF VO&NU='   '
REPLACE VO&NU WITH '600',KMN&NU WITH m_m,SVON&NU WITH S_zp/100
EXIT
ENDIF
NEXT
else
for i3=1 to 12
nu=if(i3<10,str(i3,1),str(i3,2))
if vo&nu='   '
replace vo&nu with '600',kmn&nu  WITH m_m,svon&nu with s
exit
endif
next
ENDIF
STORE 0 TO S,S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_BL,RAZN,S_ZP,S_IN,s_b
Skip
enddo
close all
clear
set device to screen
set print off
retu
