*ZarpDolg Заполнение долговременной информации 28.2.89,09:55 call curson/off-->set curs on/off
*04/10/89 18:39

parameters NextKod

SELE 0
USE SVO index svoindex
sele 0
use suder index suderind
SELE 0
USE SORG
SELE 0
USE SADRA
SET COLOR TO &STDC
CLEAR
@ 1,0
@ 1,21 SAY 'ДОЛГОВРЕМЕHHАЯ ИHФОРМАЦИЯ'

RRR=100

sele basef
STOR 9 TO SR
STOR '1' TO NU
STOR VAL(NU) TO VNU
STOR STR(SR,2) TO ST

RRR=1
 
first_lst=.t.
 
DO WHILE RRR<>0
    
   sele basef
   set color to &stdc
   if first_lst
     SET COLOR TO &STDC
     @ 2,0
     @ 2,21 SAY  NAMEPOD
     @ 2,41 SAY  basef->tn+' '+super->FIO
     @ 3,0 CLEAR
     @ 3,21 SAY '┌'+replicate('─',38)+'┐'
     @ 4,21 SAY '│ KОД ВИДА │  СУММА   │HАЧАЛО │ KОHЕЦ  │'
     @ 5,21 SAY '│HАЧИСЛЕHИЙ│   ИЛИ    │HАЧИСЛ.│HАЧИСЛ. │'
     @ 6,21 SAY '│          │% ЗА МЕСЯЦ│(ГГММ) │(ГГММ)  │'
     @ 7,21 SAY '└'+replicate('─',38)+'┘'
     @ 8,28 SAY 'H А Ч И С Л Е H И Я'
     rrr=1
   endif

   sr=9
   for N=1 to 6
     
     set color to &mcol
     nu=str(n,1)
     nvu_m=nvu&nu
     @ sr,23 say nvu_m
     set color to &mcolbln
     @ sr,21 prompt '*'
     
     set color to &stdc
     
     if first_lst
      STOR NSU&NU TO K1
      @ sr,34 SAY IF(K1=0,space(11),;
                  STR(K1/100,11,2))+'  '+NHO&NU+'  '+NKNO&NU
      sr=sr+1
      IF nvu_m<>'   '
         SELE svo
         seek nvu_m
         IF EOF()
            @ sr,21 SAY 'Kод неверен '
         ELSE
            @ sr,21 SAY NAIM
         ENDIF
      ENDI
      SR=SR+1
     else
      sr=sr+2
     endif
     sele basef
   next
   
   first_lst=.f.
   
  SET COLOR TO &ECOL
  @ 23,0 clear
  @ 23,1 SAY '┘-ввод F1-помощь {Esc}-выход {9}-предыдущий {3}-след.работник F7-удержания'
  @ 24,1 say 'F2 - лицевой счет  F3 - постоянная  F5 - расчетная информация  F6 - Архив'
  set color to &mcolbln
  PressKey=0
  set key -1 to PressCr
  set key -2 to PressCr
  set key -4 to PressCr
  set key -5 to PressCr
  set key -6 to PressCr
  set key 18 to PressCr
  set key  3 to PressCr
  set key 27 to
  set key 28 to PressCr
  
  set escape on
  menu to rrr
  set curs off
  set key -1 to
  set key -2 to
  set key -4 to
  set key -5 to
  set key -6 to
  set key 18 to
  set key  3 to
  set key 28 to
  set color to &stdc
  @ 23,0 clear
  
  IF RRR=0
     EXIT
  ENDIF
  
  do case
  
    case PressKey=-6
    
       rrr=-6
       first_lst=.t.
       DO FDLU
       first_lst=.t.
       if rrr<>-6
          exit
       else
          rrr=1
          loop
       endif
    
    case PressKey=-1 .or. PressKey=-2 .or. PressKey=-4 .or. PressKey=-5
     
       NextKod=PressKey
       exit
       
    case PressKey=18 .or. PressKey=3
       
       do DnUpSotr
       rrr=1
       loop
       
  endcase
  
  vnu=rrr
  sr=9+(rrr-1)*2
  STOR STR(VNU,1) TO NU
  
  sele basef
  
  set colo to &ecol
  @ 24,0
  
  if PressKey=28
     keyboard chr(28)
  else
     @ 24,3 say 'Вводите код начисления или нажмите: F1-помощь, {Esc}-отказ'
  endif
  
  set curs on
  
  SET COLOR TO &stdc
  spr_obl='svo'
  set key 28 to HelpKod
  r_h=sr+1
  c_h=21
  txt_h='naim'
  @ sr,23 GET NVU&NU valid(tstkdolg('basef',spr_obl,'nvu'-nu,'kvoy',r_h,c_h,txt_h,'s'))
  READ
  set key 28 to
  @ 24,0
  
  if lastkey()=27
     set curs off
     loop
  endif
  
  set color to &ucol
  @ sr,23 say nvu&nu
  set color to &stdc
  
  IF basef->NVU&NU='   '
    @ sr,0 clear to sr+1,79
    REPL NSU&NU WITH 0,NHO&NU WITH '    ',NKNO&NU WITH '    '
    set curs off
    LOOP
  ENDIF
  
  SET COLOR TO &ECOL
  @ 24,0
  @ 24,3 SAY 'Вводите данные  '
  DO WHILE .t.
   SET COLOR TO &STDC
   @ sr,32
   i=nsu&nu/100
   @ sr,34 GET i       picture '########.##'
   @ sr,46 GET NHO&NU  picture '####'
   @ sr,54 GET NKNO&NU picture '####'
   READ
   if lastkey()<>27
      repl nsu&nu with i*100
   endif
   @ sr,34 SAY STR(NSU&NU/100,11,2)+'  '+NHO&NU+'   '+NKNO&NU
   SET COLOR TO &ECOL
   @ 24,0
   if OkTstPer('nho','nkno',nu)
      exit
   endif
  ENDDO
  set curs off
  sele basef
  rrr=rrr+1
ENDDO
sele suder
use
sele sadra
use
sele sorg
use
sele svo
use
CLEAR
RETURN
*
proc FDLU

STOR '1' TO NU
SR=9
@ sr,0
sele basef

 RRR=1
 
 DO WHILE RRR<>0
   
   sele basef
   set color to &stdc
   if first_lst
     SET COLOR TO &STDC
     @ 2,0
     @ 2,18 SAY  NAMEPOD
     @ 2,41 SAY  basef->tn+' '+super->fio
     @ 3,0 CLEAR
     @ 3,0 SAY '┌'+replicate('─',78)+'┐'
     @ 4,0 say '│KОД ВИДА │  HОМЕР │  KОД   │   KОД    │КОЛ-ВО│   СУММА │   СУММА │   ПЕРИОД   │'
     @ 5,0 say '│УДЕРЖАHИЙ│ ДОK-ТА │ОРГАНИЗ.│ПОЛУЧАТЕЛЯ│ДЕТЕЙ │   ВСЕГО │   ИЛИ % │   (ГГММ)   │'
     @ 6,0 SAY '│         │        │        │АЛИМЕHТОВ │      │         │    МЕС. │ HАЧ.│KОНЕЦ │'
     @ 7,0 SAY '└'+replicate('─',78)+'┘'
     @ 8,28 SAY 'У Д Е Р Ж А H И Я'
     rrr=1
   endif
   
   sr=9
   for N=1 to 6
     sele basef
     set color to &mcol
     nu=str(n,1)
     uvu_m=uvu&nu
     @ sr,3 say uvu_m
     set color to &mcolbln
     @ sr,1 prompt '*'
             
     if first_lst
      
      set colo to &stdc
      STOR USO&NU TO K5
      STOR USU&NU TO K6
             
      @ sr,11 SAY UND&NU+' '+UKO&NU+'     '+UKPA&NU+'       '+;
                  str(UOP&NU,1)+'  '+IF(K5=0,space(11),STR(K5/100,11,2))+' '+;
                  if(k6=0,space(11),STR(K6/100,11,2))+' '+UHO&NU+' '+UKNO&NU
      
      sr=sr+1
      
      IF uvu_m<>'   '
         SELE suder
         seek uvu_m
         IF EOF()
            @ sr,0 SAY 'Kод неверен '
         ELSE
            @ sr,0 SAY NAIM
         ENDIF
      ENDI
      
      sele basef
      
      do case
         case uko&nu<>' '
            
           sele sorg
           loca for basef->uko&nu=koo
           @ sr,17 say if(found(),naim,'*неверный код*')
  
         case ukpa&nu<>' '
            
            sele sadra
            loca for basef->ukpa&nu=kap
            @ sr,46 say if(found()," "+fia,'*неверный код*')
            
      endcase
      SR=SR+1
     else
       sr=sr+2
     endif
     sele basef
   next
   
  first_lst=.f.
  SET COLOR TO &ECOL
  @ 23,0 clear
  @ 23,1 SAY '┘-ввод F1-помощь {Esc}-выход {9}-предыдущий {3}-след.работник F7-начисления'
  @ 24,1 say 'F2 - лицевой счет  F3 - постоянная  F5 - расчетная информация  F6 - Архив'
  SET COLOR TO &mcolbln
  
  PressKey=0
  set key -1 to PressCr
  set key -2 to PressCr
  set key -4 to PressCr
  set key -5 to PressCr
  set key -6 to PressCr
  set key 18 to PressCr
  set key  3 to PressCr
  set key 27 to
  set key 28 to PressCr
  set escape on
  menu to rrr
  set curs off
  set key -1 to
  set key -2 to
  set key -4 to
  set key -5 to
  set key -6 to
  set key 18 to
  set key  3 to
  set key 28 to
  set color to &stdc
  @ 23,0 clear

  IF RRR=0
    return
  ENDIF
  
  do case
  
    case PressKey=-6
    
       rrr=-6
       return
    
    case PressKey=-1 .or. PressKey=-2 .or. PressKey=-4 .or. PressKey=-5
     
       NextKod=PressKey
       rrr=0
       return
       
    case PressKey=18 .or. PressKey=3
       
       do DnUpSotr
       rrr=1
       loop
       
  endcase

  STOR 'OSIB' TO OSIB
  vnu=rrr
  STOR STR(VNU,1) TO NU
  sr=9+(rrr-1)*2
  
  sele basef
   
  set colo to &ecol
  @ 24,0
  
  if PressKey=28
     keyboard chr(28)
  else
     @ 24,3 say 'Вводите код удержания или нажмите: F1-помощь, {Esc}-отказ'
  endif
  
  set curs on
  
  SET COLOR TO &stdc
  spr_obl='suder'
  set key 28 to HelpKod
  r_h=sr+1
  c_h=0
  txt_h='naim'
  @ sr,3 GET UVU&NU valid(tstkdolg('basef',spr_obl,'uvu'-nu,'kvoy',r_h,c_h,txt_h,'s'))
  READ
  set key 28 to
  
  set color to &ucol
  @ sr,3 say uvu&nu
  set color to &stdc
  
  if lastkey()=27
     set curs off
     loop
     *repl uvu&nu with ' '
  endif
  IF basef->UVU&NU='   '
    @ sr,0 clear to sr+1,79
    REPL UND&NU WITH '        ',UKO&NU WITH '       ',UKPA&NU WITH '   ',UOP&NU WITH 0;
    USO&NU WITH 0,USU&NU WITH 0,UHO&NU WITH '    ',UKNO&NU WITH '    '
    set curs off
    LOOP
  ENDIF
  set color to &ecol
  @ 24,0
  @ 24,3 SAY 'Вводите данные  '
  set color to &stdc
  @ sr,11 GET UND&NU  && PICT "########"
  READ
  @ sr,11 SAY UND&NU
  if lastkey()=27
     set curs off
     loop
  endif
  SET COLOR TO &ECOL
  @ 24,0
  @ 24,3 SAY 'Вводите код организации или нажмите F1-помощь, {Esc}-отказ'
  SET COLOR TO &stdc
  spr_obl='sorg'
  set key 28 to HelpKod
  r_h=sr+1
  c_h=17
  txt_h='naim'
  @ sr,20 GET UKO&NU valid(tstkdolg('basef',spr_obl,'uko'-nu,'koo',r_h,c_h,txt_h))
  READ
  set key 28 to
  if lastkey()=27
      set curs off
      loop
  endif
  @ sr,20 SAY UKO&NU
  
  SET COLOR TO &ECOL
  @ 24,0
  @ 24,3 SAY 'Вводите код получателя алиментов или нажмите F1-помощь, {Esc}-отказ'
  SET COLOR TO &stdc
  spr_obl='sadra'
  set key 28 to HelpKod
  r_h=sr+1
  c_h=46
  txt_h='" "+fia'
  @ sr,33 GET UKPA&NU valid(tstkdolg('basef',spr_obl,'ukpa'-nu,'kap',r_h,c_h,txt_h))
  READ
  set key 28 to
  if lastkey()=27
     set curs off
     loop
  endif
  @ sr,33 SAY UKPA&NU

  SET COLOR TO &ECOL
  @ 24,0
  @ 24,3 SAY 'Вводите кол-во детей или {Esc}-отказ'
  SET COLOR TO &stdc
  @ sr,42 GET UOP&NU picture '9'
  READ
  if lastkey()=27
     set curs off
     loop         &&  repl uop&nu with ' '
  endif
  @ sr,42 SAY str(UOP&NU,1)
  
  SET COLOR TO &ECOL
  @ 24,0
  @ 24,3 SAY 'Вводите данные  '
  DO WHILE .t.
   SET COLOR TO &STDC
   @ sr,53
   i1=uso&nu/100
   i2=usu&nu/100
   @ sr,45 GET i1 picture '########.##'
   @ sr,57 GET i2 picture '########.##'
   @ sr,69 GET UHO&NU  picture '####'
   @ sr,74 GET UKNO&NU picture '####'
   READ
   if lastkey()<>27
      repl uso&nu with i1*100, usu&nu with i2*100
   endif
   @ sr,45 SAY STR(USO&NU/100,11,2)+' '+STR(USU&NU/100,11,2)+UHO&NU+' '+UKNO&NU
   if OkTstPer('uho','ukno',nu)
      exit
   endif
  ENDDO
  set curs off
  sele basef
  rrr=rrr+1
 ENDDO
CLEAR
RETURN
*
func OkTstPer
para p1,p2,p3
i1=val(substr(&p1&p3,3,2))
i3=val(substr(&p1&p3,1,2))
i2=val(substr(&p2&p3,3,2))
i4=val(substr(&p2&p3,1,2))
if len(trim(&p1&p3))=0
   i1=01
   i3=51
endif
if len(trim(&p2&p3))=0
   i2=12
   i4=50
endif
if i3<=50
   i3=100+i3
endif
if i4<=50
   i4=100+i4
endif
i3=190000 + i3*100 + i1
i4=190000 + i4*100 + i2
IF i3>i4 .or. i1<1 .or. i1>12 .or. i2<1 .or. i2>12
   @ 24,3 SAY 'Ошибка в дате, повторите ввод '
   return .f.
ENDIF
SET COLOR TO &STDC
@ 24,0
return .t.
*
