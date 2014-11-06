**запись прочих начислений и удержаний в базы
SET DEVICE TO SCREEN
SET CONSOLE ON
clear
@ 10,30 say '  Ж Д И Т Е !   '     
@ 12,20 say '  И Д Е Т   З А П И С Ь   И Н Ф О Р М А Ц И И '
PRIV D_N,SUM1,PODR1,TN1,V_O,MES1

store 0 to d_n,sum1
podr1='    '
tn1='      '
v_o='   '
mes1='  '

sele b
use proch
sort to proch1 on podr,tn,vo /a
close all
sele d
use proch1
go top
do while.not.eof()

podr1=podr
tn1=tn
v_o=vo
mes1=mes
d_n=dn
sum1=sum

sele 0
use base&podr1
locate for tn=tn1
if found()

if v_o<'900'
for k=1 to 12
j=if(k<10,str(k,1),str(k,2))
if vo&j='   '
replace svon&j with sum1*100,kmn&j with mes1,vo&j with v_o,dn&j with d_n*100
exit
endif
next
else
for k=1 to 12
j=if(k<10,str(k,1),str(k,2))
if ku&J='   '
replace sku&j with sum1*100,kmu&j with mes1,ku&j with v_o
exit
endif
next
ENDI
ENDIF
use
sele d
skip
enddo  
retu
set device to screen
close all
erase proch1.dbf