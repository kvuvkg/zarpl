para baz,na,opl,na2,MM
private tn1,podr1,su_3,su_36,su_610,su_113,su_18,vo1,mes1,baz1,m,t_n,s,pod,su_138,kod,so,soo
MES1=MM
m=0
s=0
kod=0
baz1=' '
ba='   '
set escape off
set safety off
store space(30) to opl1,na1
na1=na
opl1=opl       
SET CURSOR ON
CLEAR
do case
case baz='1'
kod=1
ba='520'
case baz='2'
kod=2
ba='524'
case baz='3'
kod=3
ba='527'
case baz='4'
kod=4
ba='523'
case baz='5'
kod=5
ba='525'
case baz='6'
kod=6
ba='526'
endcase                        
    store '      ' to tn1,podr1,pod
    store 0 to su_3,su_36,su_610,su_113,su_18,su_138
    select a
use po_&baz
    store reccount() to k_z
    @ 0,0 to 23,62    
    @ 2,1 to 2,62 
    @ 1,6 say na1
    @ 1,50 say 'Вид оплаты * ' 
    @ 1,66 say opl1
    @ 1,70 say '*'
    @ 3,1 clear to 22,15
    @ 3,1 say 'Подp.Таб.N' 
    @ 3,16 say na2
    @ 4,1 to 4,62
@ 24,1 say 'Месяц на котоpый начисляется доплата'
@ 24,40 say mm
    save screen to scr_1
    do while.t.
    set color to w+/n+
    @ 0,63 clear to 23,79 
    @ 0,63 to 23,79  
    set color to w+/b+
    @ 1,64 say 'ВЕДЕНИЕ'
    @ 2,64 say 'КОРРЕКТИРОВКИ'
    @ 4,64 say 'Действия клавиш:'
    @ 7,64 say 'Home-НА НАЧАЛО '
    @ 8,64 say 'End -НА КОНЕЦ  '
    @ 9,64 say '  - ВПЕРЕД ПО '
    @ 10,64 say '    ЗАПИСИ     '
    @ 11,64 say ' - НАЗАД ПО   '
    @ 12,64 say '    ЗАПИСИ     '
    @ 14,64 say 'PageDn-ВПЕРЕД  '
    @ 15,64 say 'PageUp- НАЗАД  '
    @ 16,64 say 'Insert-ВСТАВКА '
    @ 17,64 say 'Delete-УДАЛ.   '
    @ 18,64 say ' <- - ЗАМЕНА   '
    @ 19,64 SAY 'F5  - ПОИСК    '
    @ 20,64 SAY 'F4-ПЕЧАТЬ/ВИДЫ/'     
    @ 21,64 SAY 'F6-ПЕЧАТЬ/СВОД/'     
    @ 22,64 say 'ESC-ВЫХОД      '
       store 4 to a 
       store 0 to A4,a5,a6,a7,a8,a9,a10,a11,a12,a13
       store 0 to a14,a15,a16,A17,A18,A19,A20,A21,A22,v
       store ' ' to C4,c5,c6,c7,c8,c9,c10,c11,c12
       store ' ' to c13,c14,c15,c16,c17,c18,c19,c20,c21,c22
       key=inkey()
       do while A<=20.and..not.eof()
               store 'a'+ltrim(str(a,2,0)) to temp
               store recno() to &temp
tn1=tn
podr1=podr
vo1=vo
do case
case kod=1.or.kod=4
su3=su_3
su36=su_36
su610=su_610
case kod=2
su3=su_3
su36=su_36
su610=su_610
su113=su_113
su138=su_138
su18=su_18
case kod=3
su3=su_3
su36=su_36
case kod=5.or.kod=6
su3=su_3
endcase
    set color to gr+/b+
    @ a+1,1 say podr1 picture '9999'
    @ a+1,6 say tn1 picture '999999'
do case 
case kod=1.or.kod=4
    @ a+1,15 say su3 picture '9999.99'
    @ a+1,23 say su36 picture '9999.99'
    @ a+1,31 say su610 picture '9999.99'
case kod=2
    @ a+1,15 say su3 picture '9999.99'
    @ a+1,23 say su36 picture '9999.99'
    @ a+1,31 say su610 picture '9999.99'
    @ a+1,39 say su113 picture '9999.99'
    @ a+1,47 say su138 picture '9999.99'
    @ a+1,55 say su18 picture '9999.99'
case kod=3
    @ a+1,15 say su3 picture '9999.99'
    @ a+1,23 say su36 picture '9999.99'
case kod=5.or.kod=6
    @ a+1,15 say su3 picture '9999.99'
endcase
              store a+1 to a
              store v+1 to v   
              skip
              enddo
             store a4 to i
        store 0 to key
        do while key=0
        @ 24,60 say time()
        @ 24,70 say date()
        store inkey() to key
        enddo  
       do case
           case key=3.and.k_z#0
           i=a4+v
           case key=18.and.k_z#0
           store a4-v to i
           store i-20 to i
           case key=5.and.k_z<>0         &&листание по одной
            store i+1 to i               &&вперед
         case key=24.and.k_z<>0       &&листание по одной
            store i-1 to i            &&назад
     case key=7.and.k_z#0
    set color to w+/r+
    @ 23,1 clear to 24,59
    @ 23,4 SAY ' ДЛЯ УДАЛЕНИЯ ВВЕДИТЕ   *   '
    @ 24,4 say 'ДЛЯ ОТКАЗА - НАЖМИТЕ *ESC*  '
    store 4 to i
    do while i<=20
    store 'c'+ltrim(str(i,2,0)) to temp
    store 'a'+ltrim(str(i,2,0)) to temp1
    store &temp1 to y_y
    if y_y#0
    set color to w+/r+
    @ i+1,0 get &temp picture 'x' 
    endif
    store i+1 to i 
    enddo
    read
    store 4 to i
    store 4 to y
    do while i<=20
    store 'c'+ltrim(str(i,2,0)) to temp
    store &temp to ch
    if ch='*'
    store 'a'+ltrim(str(i,2,0)) to temp1
    store &temp1 to y_y
    go y_y
    delete
    store k_z-1 to k_z
     coob='                   ЗАПИСЬ УДАЛЕНА!                           '
    @ i+1,1 get coob
    set color to w+/r+
    store y+1 to y
    endif
    store i+1 to i
    enddo
    if y>4
    clear gets
    pack
    store chr(inkey(1)) to temp
    endif
    i=a4-1
    restore screen from scr_1
                 case key=22                 && вставка
a=4
               do while a<=20
    set color to w+/r+                
    @ 23,1 clear to 24,59
    @ 23,4 say ' Вводите данные '
    @ 24,4 say ' Выход - PageDn '
   store '      ' to tn1,podr1,pod
   store 0 to su_3,su_36,su_610,su_113,su_18,su_138,TN2,Q
   STORE ' ' TO Q1,Q2
    @ a+1,1 GET podr1 picture '9999'
    @ a+1,6 GET tn2 picture '999999'
do case 
case kod=1.or.kod=4
    @ a+1,15 get su_3 picture '9999.99'
    @ a+1,23 get su_36 picture '9999.99'
    @ a+1,31 get su_610 picture '9999.99'
case kod=2
    @ a+1,15 get su_3 picture '9999.99'
    @ a+1,23 get su_36 picture '9999.99'
    @ a+1,31 get su_610 picture '9999.99'
    @ a+1,39 get su_113 picture '9999.99'
    @ a+1,47 get su_138 picture '9999.99'
    @ a+1,55 get su_18 picture '9999.99'
  Case kod=3
    @ a+1,15 get su_3 picture '9999.99'
    @ a+1,23 get su_36 picture '9999.99'
case kod=5.or.kod=6
    @ a+1,15 get su_3 picture '9999.99'
endcase
READ
Q=LEN(LTRIM(STR(TN2)))
M=1
DO WHILE M<=6-Q
Q1=Q1+'0'
M=M+1
ENDDO
Q2=LTRIM(Q1)+LTRIM(STR(TN2))
TN1=Q2
if podr1='    '
EXIT
endif
append blank
              replace tn with tn1;
                      podr with podr1;
                      mes with mes1;
                         vo with opl                                     
do case 
case kod=1.or.kod=4
          REPLACE   su3 with su_3;
                      su36 with su_36;
                         su610 with su_610
case kod=2
REPLACE                  su3 with su_3;
                      su36 with su_36;
                         su610 with su_610;
                         su113 with su_113;
                         su138 with su_138;
                         su18 with su_18
case kod=3
REPLACE                   su3 with su_3;
                      su36 with su_36
case kod=5.or.kod=6
REPLACE             su3 with su_3
endcase
              store k_z+1 to k_z
              store a+1 to a
enddo          
              restore screen from scr_1
               store a4 to i  
   case key=19.and.a4#0           && замена
    save screen to scr_2
    @ 23,1 clear to 24,59
    set color to w+/r+
@ 23,1 SAY 'Укажите символом [ * ] те записи которые вы будете исправлять'
@ 24,1 say 'PageDn  -  отмена исправления                                '
SET COLOR TO GR+/B+
    store 4 to i
    do while i<=20
    store 'c'+ltrim(str(i,2,0)) to temp
    store 'a'+ltrim(str(i,2,0)) to temp1
    store &temp1 to y_y
    if y_y<>0
    @ i+1,0 get &temp picture 'x'
    endif
    store i+1 to i
    enddo
    read
    @ 23,1 clear to 24,59
    set color to w+/r+
@ 23,1 SAY 'Вводите изменения                                            '
@ 24,1 say 'ESC   -   отмена изменений                                   '
SET COLOR TO GR+/B+
    store 4 to i
    store 4 to y
    do while i<=20
    store'c'+ltrim(str(i,2,0)) to temp
    store &temp to ch
    if ch='*'
    store 'a'+ltrim(str(i,2,0)) to temp1
    store &temp1 to y_y
    go y_y
    @ i+1,1 GET podR picture '9999'
    @ i+1,6 GET tn picture '999999'
do case 
case kod=1.or.kod=4
    @ I+1,15 get su3 picture '9999.99'
    @ I+1,23 get su36 picture '9999.99'
    @ I+1,31 get su610 picture '9999.99'
READ
          REPLACE   su3 with su3;
                      su36 with su36;
                         su610 with su610
case kod=2
    @ I+1,15 get su3 picture '9999.99'
    @ I+1,23 get su36 picture '9999.99'
    @ I+1,31 get su610 picture '9999.99'
    @ I+1,39 get su113 picture '9999.99'
    @ I+1,47 get su138 picture '9999.99'
    @ I+1,55 get su18 picture '9999.99'
READ
REPLACE                  su3 with su3;
                      su36 with su36;
                         su610 with su610;
                         su113 with su113;
                         su138 with su138;
                         su18 with su18
case kod=3
    @ I+1,15 get su3 picture '9999.99'
    @ I+1,23 get su36 picture '9999.99'
READ
REPLACE                   su3 with su3;
                      su36 with su36
case kod=5.or.kod=6
    @ I+1,15 get su3 picture '9999.99'
READ
REPLACE             su3 with su3
endcase
              replace tn with tn;
                      podr with podr;
                      mes with mes1;
                         vo with opl                                     
    store y+1 to y
    endif
    store i+1 to i
    enddo
    restore screen  from scr_2
    store a4 to i
case key=-3.and.a4#0
sele d
use superlis index superpon
sele a
use po_&baz
go top
sort to zz.dbf on podr,tn /a
use zz
go top
t_n='      '
tn=t_n
set device to print
set print on
set console off
DO CASE
CASE KOD=1
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'              подpазделение '+podr
?'---------------------------------------------------------------------------------'
?'|Табельный |       Ф И О        |  До 3-х  |  От 3-х  | От 6     |    Итого     |'
?'|  номер   |                    |   лет    |  до 6 лет| до 18 лет|              |'
?'---------------------------------------------------------------------------------'
m=8
podr1=podr
store  0 to s,s1,s2,s3,s4,s5,SO,SOO
do while.not.eof()
if podr1#podr
?'|Итого                          |'+IF(S#0,str(s,10,2),'          ')+'*'+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(SOO#0,STR(SOO,14,2),'              ')
?'---------------------------------------------------------------------------------'
?''
?''
store 0 to s1,s2,s3,s4,s5,s,SO,SOO
do az
endif
pod=podr
?'|  '+tn+'  |'
t_n=tn
sele d
seek POD+t_n
??fio+'|'
sele a
??if(su3#0,str(su3,10,2),'          ') +' '+if(su36#0,str(su36,10,2),'          ')+' '+if(su610#0,str(su610,10,2),'          ')+'|'+IF(SU3#0.OR.SU36#0.OR.SU610#0,STR((SU3+SU36+SU610),14,2),'              ')+'|'
s=s+su3
s1=s1+su36
s2=s2+su610
SO=SU3+SU36+SU610
SOO=SOO+SO
m=m+1
if m>56
do az
endif
PODR1=PODR
skip
enddo
?'|Итого                          |'+IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(SOO#0,STR(SOO,14,2),'              ')
?'---------------------------------------------------------------------------------'
?''
?''
set device to screen
close all
erase zz.dbf
CASE KOD=2
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'              подpазделение '+podr
?'-----------------------------------------------------------------------------------------------------------------'
?'|Табельный |       Ф И О        |  До 3-х  |  От 3-х  | От 6     | От 10   | От 13    | Свыше    |    Итого     |'
?'|  номер   |                    |   лет    |  до 6 лет| до 10 лет|до 13 лет|до 18 лет | 18 лет   |              |'
?'-----------------------------------------------------------------------------------------------------------------'
m=8
podr1=podr
store  0 to s,s1,s2,s3,s4,s5,so,soo
do while.not.eof()
if podr1#podr
?'|Итого                          |'+IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(S3#0,str(s3,10,2),'          ')+' '+IF(S4#0,str(s4,10,2),'          ')
??' '+IF(S5#0,str(s5,10,2),'          ')+' '+str((s+s1+s2+s3+s4+s5),14,2) 
?'-----------------------------------------------------------------------------------------------------------------'
?''
?''
store 0 to s1,s2,s3,s4,s5,s,so,soo
do az
endif
pod=podr
?'|  '+tn+'  |'
t_n=tn
sele d
seek POD+t_n
??fio+'|'
sele a
??if(su3#0,str(su3,10,2),'          ') +' '+if(su36#0,str(su36,10,2),'          ')+' '+if(su610#0,str(su610,10,2),'          ')+' '+if(su113#0,str(su113,10,2),'          ')+' '+if(su138#0,str(su138,10,2),'          ')
??' '+if(su18#0,str(su18,10,2),'          ')+' '+if(su3#0.or.su36#0.or.su610#0.or.su138#0.or.su113#0.or.su18#0,str((su3+su36+su610+su138+su113+su18),14,2),'              ')
s=s+su3
s1=s1+su36
s2=s2+su610
s3=s3+su113
s4=s4+su138
s5=s5+su18
so=so+su3+su36+su610+su113+su138+su18
m=m+1
if m>56
do az
endif
PODR1=PODR
skip
enddo
?'|Итого                          |'+IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(S3#0,str(s3,10,2),'          ')+' '+IF(S4#0,str(s4,10,2),'          ')+' '
??' '+IF(S5#0,str(s5,10,2),'          ')+' '+str((s+s1+s2+s3+s4+s5),14,2) 
?'-----------------------------------------------------------------------------------------------------------------'
?''
?''
set device to screen
close all
erase zz.dbf
CASE KOD=3
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'              подpазделение '+podr
?'----------------------------------------------------------------------'
?'|Табельный |       Ф И О        |  До 13   |  От 13   |    Итого     |'
?'|  номер   |                    |   лет    | до 18 лет|              |'
?'----------------------------------------------------------------------'
m=8
podr1=podr
store  0 to s,s1,so,soo
do while.not.eof()
if podr1#podr
?'|Итого                          |'+IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+if(soo#0,str(soo,14,2),'              ')
?'----------------------------------------------------------------------'
?''
?''
store 0 to s,s1,so,soo
do az
endif
pod=podr
?'|  '+tn+'  |'
t_n=tn
sele d
seek POD+t_n
??fio+'|'
sele a
??if(su3#0,str(su3,10,2),'          ') +' '+if(su36#0,str(su36,10,2),'          ')+' '+if(su3#0.or.su36#0,str((su3+su36),14,2),'              ')
s=s+su3
s1=s1+su36
so=so+su3+su36
soo=soo+so
m=m+1
if m>56
do az
endif
PODR1=PODR
skip
enddo
?'|Итого                          |'+IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+if(soo#0,str(soo,14,2),'              ')
?'----------------------------------------------------------------------'
?''
?''
set device to screen
close all
erase zz.dbf
CASE KOD=5.or.kod=6
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'              подpазделение '+podr
?'--------------------------------------------'
?'|Табельный |       Ф И О        |  Сумма   |'
?'|  номер   |                    |          |'
?'--------------------------------------------'
m=8
podr1=podr
store  0 to s
do while.not.eof()
if podr1#podr
?'|Итого                          |'+IF(S#0,str(s,10,2),'          ')
?'--------------------------------------------'
?''
?''
store 0 to s
do az
endif
pod=podr
?'|  '+tn+'  |'
t_n=tn
sele d
seek POD+t_n
??fio+'|'
sele a
??if(su3#0,str(su3,10,2),'          ') +'|'
s=s+su3
m=m+1
if m>56
do az
endif
PODR1=PODR
skip
enddo
?'|Итого                          |'+IF(S#0,str(s,10,2),'          ')+'*'
?'--------------------------------------------'
?''
?''
set device to screen
close all
erase zz.dbf
CASE KOD=4
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'              подpазделение '+podr
?'---------------------------------------------------------------------------------'
?'|Табельный |       Ф И О        |  До 6    |  От 6    | От 13    |    Итого     |'
?'|  номер   |                    |   лет    | до 13лет | до 18 лет|              |'
?'---------------------------------------------------------------------------------'
m=8
podr1=podr
store  0 to s,s1,s2,s3,s4,s5,SO,SOO
do while.not.eof()
if podr1#podr
?'|Итого                          |'+IF(S#0,str(s,10,2),'          ')+'*'+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(SOO#0,STR(SOO,14,2),'              ')
?'---------------------------------------------------------------------------------'
?''
?''
store 0 to s1,s2,s3,s4,s5,s,SO,SOO
do az
endif
pod=podr
?'|  '+tn+'  |'
t_n=tn
sele d
seek POD+t_n
??fio+'|'
sele a
??if(su3#0,str(su3,10,2),'          ') +' '+if(su36#0,str(su36,10,2),'          ')+' '+if(su610#0,str(su610,10,2),'          ')+'|'+IF(SU3#0.OR.SU36#0.OR.SU610#0,STR((SU3+SU36+SU610),14,2),'              ')+'|'
s=s+su3
s1=s1+su36
s2=s2+su610
SO=SU3+SU36+SU610
SOO=SOO+SO
m=m+1
if m>56
do az
endif
PODR1=PODR
skip
enddo
?'|Итого                          |'+IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(SOO#0,STR(SOO,14,2),'              ')
?'---------------------------------------------------------------------------------'
?''
?''
set device to screen
close all
erase zz.dbf
ENDCASE
set device to screen
set print oFF
set console oN
case key=-5.and.a4#0
sele d
use superlis index superpon
sele a
use po_&baz
go top
sort to zz.dbf on podr,tn /a
use zz
go top
set device to print
set print on
set console off
DO CASE
CASE KOD=1
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'------------------------------------------------------------'
?'|  Подраз- |  До 3-х  |  От 3-х  | От 6     |    Итого     |'
?'|  деление |   лет    |  до 6 лет| до 18 лет|              |'
?'------------------------------------------------------------'
m=8
S_S=0
sum su3 to su_3
sum su36 to su_36
sum su610 to su_610
S_S=SU_3+SU_36+SU_610
go top
podr1=podr
store  0 to s,s1,s2,s3,s4,s5,SO,SOO
do while.not.eof()
if podr1#podr
?'|    '+podr1+'  |'
??IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(SOO#0,STR(SOO,14,2),'              ')
?'------------------------------------------------------------'
store 0 to s1,s2,s3,s4,s5,s,SO,SOO
endif
pod=podr
s=s+su3
s1=s1+su36
s2=s2+su610
SO=SU3+SU36+SU610
SOO=SOO+SO
m=m+1
PODR1=PODR
skip
enddo
GO BOTTOM
?'|    '+podr+'  |'
??IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(SOO#0,STR(SOO,14,2),'              ')
?'------------------------------------------------------------'
?'|Итого     |'
??IF(SU_3#0,str(sU_3,10,2),'          ')+' '+IF(SU_36#0,str(sU_36,10,2),'          ')+' '+IF(SU_610#0,str(sU_610,10,2),'          ')+' '+IF(S_S#0,STR(S_S,14,2),'              ')
?''
?''
set device to screen
close all
erase zz.dbf
CASE KOD=2
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'--------------------------------------------------------------------------------------------'
?'|  Подраз- |  До 3-х  |  От 3-х  | От 6     | От 10   | От 13    | Свыше    |    Итого     |'
?'|  деление |   лет    |  до 6 лет| до 10 лет|до 13 лет|до 18 лет | 18 лет   |              |'
?'--------------------------------------------------------------------------------------------'
m=8
S_S=0
sum su3 to su_3
sum su36 to su_36
sum su610 to su_610
sum su113 to su_113
sum su138 to su_138
sum su18 to su_18
S_S=SU_3+SU_36+SU_610+su_113+su_138+su_18
go top
podr1=podr
store  0 to s,s1,s2,s3,s4,s5,so,soo
do while.not.eof()
if podr1#podr
?'|    '+podr1+'  |'
??IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(S3#0,str(s3,10,2),'          ')+' '+IF(S4#0,str(s4,9,2),'         ')+' '+IF(S5#0,str(s5,10,2),'          ')
??' '+if((s+s2+s2+s3+s3+s5)#0,str((s+s1+s2+s3+s4+s5),14,2),'              ') 
?'--------------------------------------------------------------------------------------------'
store 0 to s1,s2,s3,s4,s5,s,so,soo
endif
pod=podr
s=s+su3
s1=s1+su36
s2=s2+su610
s3=s3+su113
s4=s4+su138
s5=s5+su18
so=so+su3+su36+su610+su113+su138+su18
m=m+1
PODR1=PODR
skip
enddo
GO BOTTOM
?'|    '+podr+'  |'
??IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(S3#0,str(s3,10,2),'          ')+' '+IF(S4#0,str(s4,9,2),'         ')
??' '+IF(S5#0,str(s5,10,2),'          ')+' '+if((s+s1+s2+s3+s4+s5)#0,str((s+s1+s2+s3+s4+s5),14,2),'              ') 
?'--------------------------------------------------------------------------------------------'
?'|Итого     |'
??IF(SU_3#0,str(sU_3,10,2),'          ')+' '+IF(SU_36#0,str(sU_36,10,2),'          ')+' '+IF(SU_610#0,str(sU_610,10,2),'          ')+' '
??IF(SU_113#0,str(sU_113,9,2),'         ')+' '+IF(SU_138#0,str(sU_138,10,2),'          ')+' '+IF(SU_18#0,str(sU_18,10,2),'          ')+' '+IF(S_S#0,STR(S_S,14,2),'              ')
?''
?''
set device to screen
close all
erase zz.dbf
CASE KOD=3
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'-------------------------------------------------'
?'|Табельный |  До 13   |  От 13   |    Итого     |'
?'|  номер   |   лет    | до 18 лет|              |'
?'-------------------------------------------------'
m=8
S_S=0
sum su3 to su_3
sum su36 to su_36
S_S=SU_3+SU_36
go top
podr1=podr
store  0 to s,s1,so,soo
do while.not.eof()
if podr1#podr
?'|    '+podr1+'  |'
??IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+if(so#0,str(so,14,2),'              ')
?'-------------------------------------------------'
store 0 to s,s1,so,soo
endif
pod=podr
s=s+su3
s1=s1+su36
so=so+su3+su36
m=m+1
PODR1=PODR
skip
enddo
go bottom
?'|    '+podr1+'  |'
??IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+if(so#0,str(so,14,2),'              ')
?'-------------------------------------------------'
?'|Итого     |'
??IF(SU_3#0,str(sU_3,10,2),'          ')+' '+IF(SU_36#0,str(sU_36,10,2),'          ')+' '+IF(S_S#0,STR(S_S,14,2),'              ')
?''
?''
set device to screen
close all
erase zz.dbf
CASE KOD=5.or.kod=6
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'-----------------------'
?'|  Подраз- |  Сумма   |'
?'|  деление |          |'
?'-----------------------'
m=8
S_S=0
sum su3 to su_3
go top
podr1=podr
store  0 to s
do while.not.eof()
if podr1#podr
?'|    '+podr1+'  |'
??IF(S#0,str(s,10,2),'          ')
?'-----------------------'
store 0 to s
endif
pod=podr
s=s+su3
m=m+1
PODR1=PODR
skip
enddo
GO BOTTOM
?'|    '+podr+'  |'
??IF(S#0,str(s,10,2),'          ')+'*'
?'-----------------------'
?'|Итого     |'
??IF(Su_3#0,str(su_3,10,2),'          ')
?''
?''
set device to screen
close all
erase zz.dbf
CASE KOD=4
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'------------------------------------------------------------'
?'|  Подраз- |  До 6    |  От 6    | От 13    |    Итого     |'
?'|  деление |   лет    | до 13лет | до 18 лет|              |'
?'------------------------------------------------------------'
m=8
S_S=0
sum su3 to su_3
sum su36 to su_36
sum su610 to su_610
s_S=SU_3+SU_36+SU_610
go top
podr1=podr
store  0 to s,s1,s2,s3,s4,s5,SO,SOO
do while.not.eof()
if podr1#podr
?'|    '+podr1+'  |'
??IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(SO#0,STR(SO,14,2),'              ')
?'------------------------------------------------------------'
store 0 to s1,s2,s3,s4,s5,s,SO,SOO
endif
pod=podr
s=s+su3
s1=s1+su36
s2=s2+su610
SO=so+SU3+SU36+SU610
m=m+1
PODR1=PODR
skip
enddo
GO BOTTOM
?'|    '+podr+'  |'
??IF(S#0,str(s,10,2),'          ')+' '+IF(S1#0,str(s1,10,2),'          ')+' '+IF(S2#0,str(s2,10,2),'          ')+' '+IF(SO#0,STR(SO,14,2),'              ')
?'------------------------------------------------------------'
?'|Итого     |'
??IF(SU_3#0,str(sU_3,10,2),'          ')+' '+IF(SU_36#0,str(sU_36,10,2),'          ')+' '+IF(SU_610#0,str(sU_610,10,2),'          ')+' '+IF(S_S#0,STR(S_S,14,2),'              ')
?'------------------------------------------------------------'
?''
?''
endcase
set device to screen
close all
erase zz.dbf
CASE KEY=-4.AND.A4#0  
    @ 23,1 clear to 24,59
    set color to w+/r+
@ 23,1 SAY 'Введите номер подразделения                                  '
@ 24,1 say 'Введите табельный номер                                      '
SET COLOR TO GR+/B+
podr1='    '
tn1='      '
@ 23,40 GET podr1 PICTURE '9999'
@ 24,40 GET tn1 PICTURE '999999'
READ                                                   
LOCATE FOR podr=podr1.AND.tn=tn1
IF FOUND()
STORE RECNO() TO I
ELSE
  coob =' ТАКОГО НОМЕРА ПОДРАЗ. И ТАБЕЛЬН.НОМЕРА НЕТ  '
  COOB1='                                             '
SET COLOR TO W+/R+
@ 23,1 CLEAR TO 24,59
    @ 23,1 get coob
    @ 24,1 GET COOB1
    store chr(inkey(1)) to coob,COOB1
SET COLOR TO GR+/B+
ENDIF 
                 case key=27             &&конец работы
SET CURSOR OFF
CLOSE ALL
            return
                  case key=1.and.k_z<>0    &&на начало
                     i=1
                  case key=6.and.k_z<>0    && на конец 
                     i=k_z

                  case key=-2 .and.k_z#0           &&твердая копия

                    otherwise      &&другие клавиши
                       i=a4
                  endcase
                  restore screen from scr_1
                    if i<1
                      i=1
                    endif
                    if i>k_z
                      i=k_z
                    endif
                    if i#0
                      go i
                    endif
               enddo
retu
proc az
do case
case kod=1
?''
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'              подpазделение '+podr
?'---------------------------------------------------------------------------------'
?'|Табельный |       Ф И О        |  До 3-х  |  От 3-х  | От 6     |    Итого     |'
?'|  номер   |                    |   лет    |  до 6 лет| до 18 лет|              |'
?'---------------------------------------------------------------------------------'
m=8
case kod=2
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'              подpазделение '+podr
?'-----------------------------------------------------------------------------------------------------------------'
?'|Табельный |       Ф И О        |  До 3-х  |  От 3-х  | От 6     | От 10   | От 13    | Свыше    |    Итого     |'
?'|  номер   |                    |   лет    |  до 6 лет| до 10 лет|до 13 лет|до 18 лет | 18 лет   |              |'
?'-----------------------------------------------------------------------------------------------------------------'
m=8
case kod=3
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'              подpазделение '+podr
?'----------------------------------------------------------------------'
?'|Табельный |       Ф И О        |  До 13   |  От 13   |    Итого     |'
?'|  номер   |                    |   лет    | до 18 лет|              |'
?'----------------------------------------------------------------------'
M=8
case kod=4
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'              подpазделение '+podr
?'--------------------------------------------'
?'|Табельный |       Ф И О        |  Сумма   |'
?'|  номер   |                    |          |'
?'--------------------------------------------'
M=8
case kod=5
?' '
?'              '+na1 
?'              за '+mesname+' '+tek_god+' года'
?'              подpазделение '+podr
?'---------------------------------------------------------------------------------'
?'|Табельный |       Ф И О        |  До 6    |  От 6    | От 13    |    Итого     |'
?'|  номер   |                    |   лет    | до 13лет | до 18 лет|              |'
?'---------------------------------------------------------------------------------'
m=8
endcase
retu
