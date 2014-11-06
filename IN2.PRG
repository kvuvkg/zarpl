set device to screen
priv mes,mes1,k,s,m,li,tn1,tn2,kk,vv,v_v,v_v1,v_v2,k,gt,gb,i,f_io,vv3,vv4,v_v3,v_v4,M_M,us,un,ur
PRIV S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_BL,S_ZP,RAZN,s_b,S_IN,x1,x2
store 0 to z,z1,z2,z3,z4,us,un,ur,x1,x2
M_M='  '
sele f
use ssp
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
@ 1,1 say '¥à¥áç¥â ¯®¤®å®¤­®£® ­ «®£  ¯® ®¤­®¬ã ¨«¨ £pã¯¯¥ â ¡¥«ì­ëå ­®¬¥p®¢'
set color to &stdc
@ 3,5 SAY '‚¢¥¤¨â¥ ­®¬¥à ¯®¤à §¤¥«¥­¨ï ¤«ï ª®â®p®£® á¤¥« âì à áç¥â '
@ 5,5 get kk picture '9999' 
@ 23,0 clear
@ 23,1 say ' -¢¢®¤ F1-¯®¬®éì {Esc}-¢ëå®¤ '
@ 24,1 say ' '
read
if lastkey()=27
retu
endif
@ 5,20 say '®¤p §¤¥«¥­¨¥'
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
@ 7,5 say '‚¢¥¤¨â¥ ¬¥áïæ §  ª®â®pë© ­ã¦­® ¢®§¢à â¨âì ­ «®£'
@ 9,5 get per picture '99.99-99.99'
@ 11,5 SAY '‚¢¥¤¨â¥ â ¡¥«ì­ë© ­®¬¥p ¨«¨ £pã¯¯ã â ¡¥«ì­ëå ­®¬¥p®¢'
@ 12,5 say 'ˆ¬¥îé¨¥áï â ¡¥«ì­ë¥ ­®¬¥p  ­ å®¤ïâáï ¢ ¯p¥¤¥« å ['+gt+'-'+gb+']'
@ 15,5 GET TNN PICTURE '999999-999999'
  @ 23,0 clear
  @ 23,1 SAY 'Ù-¢¢®¤ F1-¯®¬®éì {Esc}-¢ëå®¤ '
  @ 24,1 say ''
read
@ 23,0 CLEAR
@ 23,1 SAY '† „ ˆ ’ …!'
set cursor on
if lastkey()=27
retu
endif
set console on
TN1=SUBSTR(TNN,1,6)
TN2=SUBSTR(TNN,8,6)
IF TN2=' '
TN2=TN1
ENDIF
mes=val(substr(per,1,2))  && ¬¥áïæ ¨­¤¥ªá æ¨¨
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
for i=1 to 12
   nu=if(i<10,str(i,1),str(i,2))
   if kmn&nu=m_m.and. vo&nu='630'
s=s+svon&nu
endif
next
ur=int(s/100)*0.12
if ur#0
for i=1 to 12
   nu=if(i<10,str(i,1),str(i,2))
if ku&nu='   '
replace ku&nu with '990',kmu&nu with m_m,sku&nu with ur*100
ur=0
s=0
exit
endif
next 
endif
skip
enddo
SET CONFIRM OFF
return
