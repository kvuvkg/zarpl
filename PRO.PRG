private tn1,podr1,sum,vo1,mes1,dn1,tn2,q1,q2
set escape off
set safety off
set cursor on
clear
    store '      ' to tn1,podr1,mes1,vo1
    store 0 to sum1,dn1
    select a
use proch
    store reccount() to k_z
   @ 0,0 to 23,62    
   @ 2,1 to 2,62 
   @ 1,6 say ' П р о ч и е    в и д ы    о п л а т '
   @ 3,1 clear to 22,15
   @ 3,1 say 'Подp. Таб.N   Вид оп Мес. Дни       Сумма' 
   @ 4,1 to 4,62
@ 24,1 SAY 'При вводе и замене для удержаний часы не проставлять'
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
    @ 20,64 SAY 'F6-ЧИСТКА БАЗЫ '     
    @ 21,64 SAY 'В ПОДРАЗДЕЛЕНИЯ'     
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
dn1=dn
sum1=sum
mes1=mes
    @ a+1,1 say podr1 picture '9999'
    @ a+1,8 say tn1 picture '9999999'
    @ a+1,16 say vo1 picture '999'
    @ a+1,22 say mes1 picture '99'
    @ a+1,26 say dn1 picture '999.99'
    @ a+1,33 say sum1 picture '9999999.99'
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
   store '      ' to tn1
store '    ' to podr1
vo1='   '
mes1='  '
   store 0 to sum1,dn1,tn2
   STORE ' ' TO Q1,Q2
    @ a+1,1 GET podr1 picture '9999'
    @ a+1,8 GET tn2 picture '999999'
    @ a+1,16 get vo1 picture '999'
    @ a+1,22 get mes1 picture '99'
    @ a+1,26 get dn1 picture '999.99'
    @ a+1,33 get sum1 picture '9999999.99'
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
                         vo with vo1;
                      dn with dn1;
                      sum with sum1                                     
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
podr1=podr
tn1=tn
vo1=vo
dn1=dn
mes1=mes
sum1=sum
    @ i+1,1 GET podr1 picture '9999'
    @ i+1,8 GET tn1 picture '999999'
    @ I+1,16 get vo1 picture '999'
    @ I+1,22 get mes1 picture '99'
    @ I+1,26 get dn1 picture '999.99'
    @ i+1,33 get sum1 picture '9999999.99'
READ
REPLACE                  podr with podr1;
                      tn with tn1;
                         vo with vo1;
                         mes with mes1;
                         dn with dn1;
                         sum with sum1
    store y+1 to y
    endif
    store i+1 to i
    enddo
    restore screen  from scr_2
    store a4 to i
case key=-5.and.a4#0
ZAP
*save screen to sz1
*do pro1
*restore screen from sz1
case key=-4.and.a4#0
@ 23,1 clear to 24,59
set color to w+/r+
@ 23,1 say 'Введите номер подразделения'
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
set device to screen
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
