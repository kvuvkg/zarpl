PARA Q,W,E,R,T
kol=0
SET CURSOR ON
CLEAR
SELE d
use skl
SORT TO SKL1 ON CEX,TN /A
USE SKL1
close all
erase skl.dbf
rename skl1.dbf to skl.dbf
sele d
use skl
SUM SZP TO S_S1
SUM PR TO S_S2
sum spr to s_s5
SUM (SZP+PR) TO S_S4
   store '      ' to tn1,cex1
   store '                    ' to fio1
   store 0 to ch1,kty1,proc1,szp1,pr1,spr1,fond1,deproc1
   store reccount() to k_z
   SET COLOR TO G+/N
    @ 0,0 to 24,79 double    
    @ 2,1 to 2,78  double
    @ 5,1 to 5,78 DOUBLE
    @ 20,1 to 20,25 double
    @ 20,49 TO 20,78 DOUBLE
    SET COLOR TO BG+/N
    @ 3,1 say 'ТАБЕЛ.|ФАМИЛИЯ,ИМЯ,ОТЧЕСТВО|ПОДР|ОТР.  |К Т У | % | % | СУММА |ПРИРА- | СУММА'
    @ 4,1 SAY 'НОМЕР |                    |    |ЧАСОВ |      |ПРМ|ДПР| З/ПЛ  |БОТОК  | ПРЕМИИ'
    @ 20,27 say 'ВЕДЕНИЕ КОРРЕКТИРОВКИ'
    SET COLOR TO G+/N
    save screen to scr_1
    do while.t.
    SET COLOR TO w+/N
    @ 21,2 say ' - Вперед по записи'
    @ 22,2 say ' - Назад  по записи'
    @ 23,2 say '<-- Замена          '
    @ 21,23 to 23,23 double
    @ 21,25 say 'Home - На начало'
    @ 22,25 say 'End  - На конец '
    @ 23,25 say ' ->  - Поиск    '
    @ 21,41 to 23,41 double
    @ 21,43 say 'PageDn - Вперед'
    @ 22,43 say 'PageUp - Назад '
    @ 23,43 SAY 'F3 - Печать    '
    @ 21,59 to 23,59 double
    @ 21,61 say 'Insert - Вставка '
    @ 22,61 say 'Delete - Удаление'
    @ 23,61 say 'ESC - Конец работы' 
       store 6 to a 
       store 0 to a6,a7,a8,a9,a10,a11,a12,a13
       store 0 to a14,a15,a16,A17,A18,A19,A20,A21,A22,v
       store ' ' to c6,c7,c8,c9,c10,c11,c12
       store ' ' to c13,c14,c15,c16,c17,c18,c19,c20,c21,c22
       key=inkey()
       do while A<=17.and..not.eof()
               store 'a'+ltrim(str(a,2,0)) to temp
               store recno() to &temp
tn1=tn
cex1=cex                                 
fio1=fio                       
ch1=ch
kty1=kty
proc1=proc
szp1=szp
pr1=pr
spr1=spr
deproc1=deproc
cex1=cex
fond1=fond 
SET COLOR TO gr+/N
    @ a,1 say tn1 picture '999999'
    @ a,8 say fio1 picture 'xxxxxxxxxxxxxxxxxxxx'
    @ a,29 say cex1 picture '9999'
    @ a,34 say ch1 picture '999.99'
    @ a,41 say kty1 picture '99.99'
    @ a,48 say proc1 picture '999'
    @ a,52 say deproc1 picture '999'
    @ a,56 say szp1 picture '9999.99'
    @ a,64 say pr1 picture '9999.99'
    @ a,72 say spr1 picture '9999.99'
             store a+1 to a
             store v+1 to v   
             skip
             enddo
SET COLOR TO W+/N
@ A,3 SAY "ИТОГО"
@ A,55 SAY S_S1 PICTURE '99999.99'
@ A,63 SAY S_S2 PICTURE '99999.99'
@ A+1,3 SAY 'ИТОГО ФОНД'
@ A+1,20 SAY S_S4 PICTURE '999999.99'
SET COLOR TO GR+/N
             store a6 to i
        store 0 to key
        do while key=0
       @ 24,60 say time()
       @ 24,70 say date()
        store inkey() to key
        enddo  
       do case
           case key=3.and.k_z#0
           i=a6+v
           case key=18.and.k_z#0
           store a6-v to i
           store i-17 to i
           case key=5.and.k_z<>0         &&листание по одной
            store i+1 to i               &&вперед
         case key=24.and.k_z<>0       &&листание по одной
            store i-1 to i            &&назад
     case key=7.and.k_z#0
    set color to w+/r+,n+/w+
    @ 21,2 clear to 23,78
    @ 22,8 SAY ' Для удаления введите  " * "  < - >  Для отказа   нажмите PgDn      '
    @ 20,26 SAY ' РЕЖИМ УДАЛЕНИЯ ЗАПИСЕЙ '
    store 6 to i
    do while i<=17  
    store 'c'+ltrim(str(i,2,0)) to temp
    store 'a'+ltrim(str(i,2,0)) to temp1
    store &temp1 to y_y
    if y_y#0
    set color to w+/r+
    @ i,0 get &temp picture 'x' 
    endif
    store i+1 to i 
    enddo
    read
    store 6 to i
    store 6 to y
    do while i<=17
    store 'c'+ltrim(str(i,2,0)) to temp
    store &temp to ck
    if ck='*'
    store 'a'+ltrim(str(i,2,0)) to temp1
    store &temp1 to y_y
    go y_y
    delete
    store k_z-1 to k_z
    coob='                                     ЗАПИСЬ УДАЛЕНА!                           '
    @ i,0 get coob
    set color to w+/n+
    store y+1 to y
    endif
    store i+1 to i
    enddo
    if y>6
    clear gets
    pack
    store chr(inkey(1)) to temp
    endif
    i=a6-1
    restore screen from scr_1
               case key=22                 && вставка
  a=6
               do while a<=17
    set color to w+/r+                
    @ 21,2 clear to 23,78
    @ 22,4 say ' Вводите необходимую информацию '
    @ 23,4 say ' Выход - PageDn '
    @ 20,26 SAY ' РЕЖИМ ВСТАВКИ ЗАПИСЕЙ '
   store '      ' to tn1
   store '    ' to cex1
   store '                    ' to fio1
   store 0 to kty1,proc1,szp1,pr1,spr1,deproc1,fond1,ch1
    @ a,1 get tn1 picture '999999'
    @ a,8 get fio1 picture 'xxxxxxxxxxxxxxxxxxxx'
    @ a,29 get cex1 picture '9999'
    @ a,34 get ch1 picture '999.99'
    @ a,41 get kty1 picture '99.99'
    @ a,48 get proc1 picture '999'
    @ a,52 get deproc1 picture '999'
    @ a,56 get szp1 picture '9999.99'
    @ a,64 get pr1 picture '9999.99'
    @ a,72 get spr1 picture '9999.99'
read
if tn1='     '
exit
endif
              append blank
              replace  tn   with    tn1;  
                       fio  with    fio1   
              replace  cex  with    cex1;
                       ch   with    ch1;
                       kty  with    kty1;
                       proc with    proc1; 
                       szp  with    szp1
             replace   pr   with    pr1; 
                       spr  with    spr1;
                       deproc with deproc1  
store a+1 to a
store k_z+1 to k_z
                   enddo
              restore screen from scr_1
              store a6 to i  
    case key=19.and.a6#0           && замена
    save screen to scr_2
    @ 21,2 clear to 23,78
    set color to w+/r+
@ 22,4 SAY ' Укажите символом [ * ] те записи которые вы будете исправлять'
@ 23,4 say ' PageDn  -  отмена исправления                                '
@ 20,26 SAY ' РЕЖИМ ЗАМЕНЫ ЗАПИСЕЙ '
    store 6 to i
    do while i<=17
    store 'c'+ltrim(str(i,2,0)) to temp
    store 'a'+ltrim(str(i,2,0)) to temp1
    store &temp1 to y_y
    if y_y<>0
    @ i,0 get &temp picture 'x'
    endif
    store i+1 to i
    enddo
    read
    store 6 to i
    store 6 to y
    do while i<=17
    store'c'+ltrim(str(i,2,0)) to temp
    store &temp to ck
    if ck='*'
    store 'a'+ltrim(str(i,2,0)) to temp1
    store &temp1 to y_y
    go y_y
tn1=tn
cex1=cex                                 
fio1=fio                       
ch1=ch
kty1=kty
proc1=proc
szp1=szp
pr1=pr
spr1=spr
deproc1=deproc
cex1=cex
fond1=fond 
    @ i,1 get tn1 picture '999999'
    @ i,8 get fio1 picture 'xxxxxxxxxxxxxxxxxxxx'
    @ i,29 get cex1 picture '9999'
    @ i,34 get ch1 picture '999.99'
    @ i,41 get kty1 picture '99.99'
    @ i,48 get proc1 picture '999'
    @ i,52 get deproc1 picture '999'
    @ i,56 get szp1 picture '9999.99'
    @ i,64 get pr1 picture '9999.99'
    @ i,72 get spr1 picture '9999.99'
read
if tn1='     '
exit
endif
              replace  tn   with    tn1;  
                       fio  with    fio1   
              replace  cex  with    cex1;
                       ch   with    ch1;
                       kty  with    kty1;
                       proc with    proc1; 
                       szp  with    szp1
             replace   pr   with    pr1; 
                       spr  with    spr1;
                       deproc with deproc1
    store y+1 to y
    endif
    store i+1 to i
    enddo
    restore screen  from scr_2
    store a6 to i
CASE KEY=4.AND.A6#0  
    @ 21,2 clear to 23,78
    set color to w+/r+
@ 20,26 SAY ' РЕЖИМ ПОИСКА ЗАПИСЕЙ '
@ 22,4 SAY ' Введите табельный номер для поиска работающего               '
tn1='      '
@ 23,20 GET tn1 PICTURE '999999'
READ                                                   
LOCATE FOR tn=tn1
IF FOUND()
STORE RECNO() TO I
ELSE
  coob ='                      ТАКОЙ  ЗАПИСИ НЕТ                                    '
SET COLOR TO W+/R+
@ 21,2 CLEAR TO 23,78
    @ 22,4 get coob
    store chr(inkey(1)) to coob
ENDIF 
case key=-2
@ 10,30 clear to 12,50
@ 10,30 to 12,67 double
@ 11,34 say ' Ждите!Идет печать ведомости! '
set console off
set device to print 
set print on
sele d
use skl
?''
?'                      Ведомость распределения з/пл по бригаде     '
?'                                  за '+r
?''
?'Расчетный фонд з/платы    '+str(fond)
?'-------------------------------------------------------------------------------------------------------'
?'|ТАБЕЛ.|ФАМИЛИЯ,ИМЯ,ОТЧЕСТВО|ПОДР|ОТР.  |К Т У | % | % |   СУММА | ПРИРА- |  СУММА  |  К Т У |  %     |'
?'|НОМЕР |                    |    |ЧАСОВ |      |ПРМ|ДПР|   З/ПЛ  | БОТОК  |  ПРЕМИИ |        | ПРЕМИИ |'
?'-------------------------------------------------------------------------------------------------------'
kol=9
go top
do while.not.eof()
?'|'+tn+'|'+fio+'|'+cex+'|'+str(ch,6,2)+'|'+str(kty,6,2)+'|'+str(proc,3)+'|'+str(deproc,3)+'|'+str(szp,9,2)+'|'+str(pr,8,2)+'|'+str(spr,9,2)
?'-------------------------------------------------------------------------------------------------------'
if kol>=45
?''
?''
?''
?'-------------------------------------------------------------------------------------------------------'
?'|ТАБЕЛ.|ФАМИЛИЯ,ИМЯ,ОТЧЕСТВО|ПОДР|ОТР.  |К Т У | % | % |   СУММА | ПРИРА- |  СУММА  |  К Т У |   %    |'
?'|НОМЕР |                    |    |ЧАСОВ |      |ПРМ|ДПР|   З/ПЛ  | БОТОК  |  ПРЕМИИ |        | ПРЕМИИ |'
?'-------------------------------------------------------------------------------------------------------'
kol=7
endif
kol=kol+2
skip
enddo
?''
?'|  Итого                                             '+str(s_s1,11,2)+'* '+str(s_s2,8,2)+'* '+str(s_s5,8,2)+'*'
?''
?''
?''
set device to screen
set print off
set console on
                 case key=27             &&конец работы
close all
           return
                  case key=1.and.k_z<>0    &&на начало
                     i=1
                  case key=6.and.k_z<>0    && на конец 
                     i=k_z
                  case key=-2 .and.k_z#0           &&твердая копия
                    otherwise      &&другие клавиши
                       i=a6
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
