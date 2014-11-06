*** 19.06.92
*** Работа с аpхивом
*** s_zpl.prg
*PARA a
CLEAR
priv mes,mes1,k,s,m,li,tn1,tn2,kk,vv,v_v,v_v1,v_v2,k,gt,gb,i,f_io,vv3,vv4,v_v3,v_v4
sele a
use svo
go top
vv='*'
v2=' '
do while.not.eof()
if kps#'1'
vv=vv+kvoy+'*'
endi
skip
enddo

sele f
use ssp
sele d
use superlis index superpon
@ 0,0 CLEAR
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
@ 1,1 say 'Сведения о з/пл по одному цеху по одному или гpуппе табельных номеpов'
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

@ 7,5 say 'Введите пеpиод за котоpый нужны сведения о з/плате'
@ 9,5 get per picture '99.99-99.99'
@ 11,5 SAY 'Введите табельный номеp или гpуппу табельных номеpов'
@ 12,5 say 'Имеющиеся табельные номеpа находятся в пpеделах ['+gt+'-'+gb+']'
@ 15,5 GET TNN PICTURE '999999-999999'
@ 17,5 say 'Введите виды оплат'
@ 18,2 GET VV1 PICTURE '*999*999*999*999*999*999*999*999*999*999*999*999*999'
@ 19,2 get vv2 picture '*999*999*999*999*999*999*999*999*999*999*999*999*999'
@ 20,2 get vv3 picture '*999*999*999*999*999*999*999*999*999*999*999*999*999'
@ 21,2 get vv4 picture '*999*999*999*999*999*999*999*999*999*999*999*999*999'


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

vv=vv1+vv2+vv3+vv4
for i=2 to 50 step 4
if substr(vv1,i,3)#' '
v_v1=v_v1+SUBSTR(vv1,I,3)+' '
endif
next
for i=2 to 50 step 4
if substr(vv2,i,3)#' '
V_V2=V_V2+SUBSTR(VV2,I,3)+' '
ENDIF
NEXT
for i=2 to 50 step 4
if substr(vv3,i,3)#' '
v_v3=v_v3+SUBSTR(vv3,I,3)+' '
endif
next
for i=2 to 50 step 4
if substr(vv4,i,3)#' '
v_v4=v_v4+SUBSTR(vv4,I,3)+' '
endif
next

TN1=SUBSTR(TNN,1,6)
TN2=SUBSTR(TNN,8,6)
IF TN2=' '
TN2=TN1
ENDIF

mes=val(substr(per,1,2))  
mes1=val(substr(per,7,2))
set console off
set device to printer
set print on
set printer to a.txt

?'                 Сведения о заработной плате работников'
?'               подpазделения'+' '+kk+' за период'+'  '+per
?'                           по видам оплат'
?''
?' '+ltrim(v_v1)
m=m+5
IF LTRIM(V_V2)#' '
?' '
?' '+LTRIM(v_v2)
m=m+2
ENDIF
IF LTRIM(V_V3)#' '
?' '
?' '+LTRIM(v_v3)
m=m+2
ENDIF
IF LTRIM(V_V4)#' '
?' '
?' '+LTRIM(v_v4)
m=m+2
ENDIF


?'                                            Лист 1'
?'|---------------------------------------------------------|'
?'| Подраз-| Табельный | Фамилия,имя,отчество|  С у м м а   |'
?'| деление|   номер   |                     |              |'
?'|---------------------------------------------------------|'
m=m+5
sele a
*use base&kk
set filter to tn>=tn1.and.tn<=tn2
GO TOP

do while.not.eof()
i=recno()
sele b
use seba&kk
*go (i-1)*12+mes  && если необходимо с опеделенного месяца ане с начала базы
go (i-1)*12+1
K=13
i1=1
DO WHILE K-i1#0
*DO WHILE K-mes#0

for i=1 to 12
nu=if(i<10,str(i,1),str(i,2))
if (kmn&nu>=str(mes).and.'*'+VO&NU$VV).OR.(KMN&NU<=STR(MES1).AND.'*'+VO&NU$VV)
s=s+svon&nu
endif
next
SKIP
*MES=MES+1
i1=i1+1
ENDDO
MES=VAL(SUBSTR(PER,1,2))
sele d
seek (kk+base&kk->tn)
f_io=fio
sele a
if s#0
?'|  '+kk+'  |   '+tn+'  | '+f_io+'| '+str(s/100,13,2)+'|'
?'|---------------------------------------------------------|'
m=m+2
endif
s=0
if m>=62
li=li+1
do sc.prg
endif
Skip
enddo
if m<=60
for i=m to 60
?' '
next
endif
?'|----------------------------------------------------------'
?''
?''
m=0
set device to print
set print to
type a.txt to print
clear
close all
*erase a.txt
set device to screen
retu

proc sc
?' '
?' '
?' '
?'                                            Лист '+ltrim(str(li))
?'|---------------------------------------------------------|'
?'| Подраз-| Табельный | Фамилия,имя,отчество|  С у м м а   |'
?'| деление|   номер   |                     |              |'
?'|---------------------------------------------------------|'
m=8
retu

