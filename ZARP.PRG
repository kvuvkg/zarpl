para ZarpPara
priv ParaZarp
SET SCOR OFF
SET DATE BRIT
SET HEAD OFF
SET SAFE OFF
SET TALK OFF
Set deleted on
set escape on
close data
set curs off
set proc to ZarpProc
set proc to SongOper
set key -38 to DosExit
frame   = '┌─┐│┘─└│'
framedbl= '╔═╗║╝═╚║'
ppp=gete('ZPS_DIR')
set path to &ppp
rele ppp
if type("ZarpPara")<>"C"
   ParaZarp=""
else
   ParaZarp=upper(ZarpPara)
endif
SongSong=if("/P" $ ParaZarp, .t. , .f. )
Nar_MPO=if("/NAR" $ ParaZarp, .t. , .f. )
macro_MPO=''
if file('color.mem') .and. ( "/SC" $ ParaZarp .or. "/+SC" $ ParaZarp )
   NewZarp=ParaZarp
   rest from color.mem additive
   ParaZarp=NewZarp
   rele NewZarp, ZarpPara
else
   if (iscolor() .or. '/C' $ ParaZarp) .and. (.not. ('/M' $ ParaZarp))
      mcolbln='w/w,w+*/r'
      mcolbln1='w+/n,w+*/r'
      stdc='w/,w+/br'
      mcol='/w,w+/r'
      ecol='w+/r,/w'
      bcol='gr+/,/w'
      ucol='/bg,w+/'
   else
      if iscolor()
         stor 'w+/,/w' to stdc,mcol,bcol,ucol
         mcolbln='w/w,w+*/'
         mcolbln1='w+/n,n*/w'
         ecol='/w,w+/'
      else
         stor '' to stdc,bcol
         mcol='w,i'
         mcolbln='n/n,w+*/'
         mcolbln1='w+/n,i*'
         stor "i,w" to ecol,ucol
      endif
   endif
endif
if '/+SC' $ ParaZarp
   do MySetCol
   save to color.mem
endif
ktu_BRIG=.f.
if '/W:' $ ParaZarp
   ktu_LKB =val(substr(ParaZarp,at('/W:',ParaZarp)+3,1))
   if ktu_LKB>0
      ktu_BRIG=.t.
   endif
endif
setcolor(stdc)
level1=1
do FirstPic
if SongSong
   do StartSong
endif
do ProtCT
if .not. AskPassW()
   return
endif
SprParm='SprParm'

if '/I' $ ParaZarp .or. ! file('superpon.ntx') .or. ! file('supertn.ntx');
                   .or. ! file('svoindex.ntx') .or. ! file('suderind.ntx')
   do IndexSup
   use svo
   index on kvoy to svoindex
   use suder
   index on kvoy to suderind
use nalog
index on mes to nal_og
   close data
endif
use dopsved
prod_rd  =prd
Min_Raz_A=minra
DOPS_MAXS=if(type('maxs')='N',maxs*100,0)
DOPS_MAXR=if(type('maxr')='N',maxr*100,0)
DOPS_RK  =if(type('rk')  ='N',rk      ,0)
dops_ukopoch=if(type('ukopoch')="C",dopsved->ukopoch,'')
use
use ssp
go top
SprTop=if(eof(),-999,recno())
SprNom=1
use
do ReadSRDs
zarp_fst=.t.
DO WHILE .t.
   set curs off
   if zarp_fst
     zarp_fst=.f.
   else
     do FirstPic
   endif
   setcolor(bcol)
   @ 01,01,03,22 box frame+' '
   @ 04,01,06,22 box frame+' '
   @ 01,23,03,50 box frame+' '
   @ 04,23,06,50 box frame+' '

   setcolor(mcol)
   @ 8,0,11,79 box replicate('▒',9)  &&frame+'▒'  │
   setcolor(ecol)
   @ 10,1 say;
' {стрелки}-движение {Esc}/{Ключ}-конец ┘-выбор  F10-руководcтво пользователя '
   setcolor(mcol)
   set message to 9
   @ 9,0
   @ 02,2  prompt "Обработка информации";
           message  " Выбор подразделения    Ввод оперативной информации    Печать выходных форм     "
   @ 05,2  PROMPT "Коррект.справочников";
           message  " Создание, изменение cтруктуры, удаление и корректировка справочников           "
   @ 02,24 PROMPT " Технологические операции ";
           message  " Регистрация и удаление подразделения      Установка и переход к новому месяцу  "
   @ 05,24 prompt "   Корректировка пароля   ";
           message  " Изменение пароля  ***** Рекомендуем делать не реже одного раза в меcяц *****   "
   PressKey=0
   SET KEY 27 TO
   set key 19 to left_shi
   set key  4 to rigth_shi
   set key 18 to NullKeyB
   set key  3 to NullKeyB
   set key -9 to PressCr
   keyboard ''
   MENU to level1
   set key 19 to
   set key  4 to
   set key 18 to
   set key  3 to
   set key -9 to
   set message to
   @ 10,1,10,78 box replicate('▒',9)
   if level1<>0
      setcolor(ecol)
      @ 10,33 say '...Ждите...'
   endif
   setcolor(stdc)
   if PressKey=-9
      do UserGuid
      loop
   endif
   open_a_b=.f.
   move_chrs = CHR(24) + CHR(5) + CHR(3) + CHR(18) + CHR(31) + CHR(30)
   if level1=0
      r_exit=' '
      do AskYesNo with r_exit,10,' Вы хотите закончить работу ? ',ecol
      if r_exit='D'
         exit
      endif
   endif
   if level1=1 .or. level1=3
      sele 0
      use ssp
      go top
      SprTop=if(eof(),-999,recno())
      SprNom=1
      use
   endif
   do while .t.
    old_level=level1
    DO CASE
      case level1=1
         sele 0
         use ssp
         count all for z='+' to all_zak
         go SprTop
         skip SprNom-1
         if z<>'+'
            go top
            SprTop=if(eof(),-999,recno())
            SprNom=1
         endif
         use
         do ZarpInfo with level1
      CASE LEVEL1=2
          DO SprMengr with SprParm,level1
      CASE LEVEL1=3
          DO FormBase with level1
      case level1=4
         if AskPassW()
            do AskNewP
         endif
    ENDCASE
    setcolor(stdc)
    if old_level=level1
       exit
    endif
   enddo
enddo
RELE ALL
close data
close proc
CLEAR
set curs on
RETURN
*
proc Left_Shi
para p1,p2,p3
keyboard chr(5)+chr(5)
return
*
proc Rigth_Shi
para p1,p2,p3
keyboard chr(24)+chr(24)
return
*
function AskPassW
priv pass,i,j
set curs off
set color to /
@ 0,0 clear to 11,79
sele 0
i=0
do while .not.file('password.dbf') .and. i<>27
   setcolor(bcol)
   @ 1,10,3,60 box framedbl+' '
   setcolor(ecol)
   @ 2,12 say 'Уcтановите рабочий диcк или нажмите {Esc}/{Ключ}-отказ'
   i=inkey(0)
enddo
if i=27
  sele a
  return .f.
endif
use password
if trim(passw)==''
   use
   return .t.
endif
setcolor(bcol)
@ 1,10,3,60 box framedbl+' '
setcolor(ecol)
@ 2,12 say 'Введите пароль или нажмите {Esc}/{Ключ}-отказ'
j=col()+1
set color to n/n,n/n
pass=space(len(passw))
i=0
do while pass<>passw .and. i<>27
   keyboard ''
   @ 2,j get pass
   read
   i=lastkey()
enddo
setcolor(stdc)
use
sele a
set color to /
@ 0,0 clear to 11,79
return if(i=27,.f.,.t.)
*
procedure AskNewP
priv pass,pass1,i,j,Scr
scr=savescreen(5,10,11,64)
sele b
use password
setcolor(bcol)
@ 5,10,11,64 box framedbl+'▒'
setcolor(ecol)
@ 6,12 say 'Введите "новый" пароль 2 раза или нажмите {Esc}/{Ключ}-отказ'
@ 8,12 say 'Поле для первого ввода '
@ 9,12 say 'Поле для второго ввода '
j=col()
set color to n/n,n/n
stor space(len(passw))  to pass,pass1
i=0
do while (pass=space(len(passw)) .or. pass<>pass1) .and. i<>27
   @ 8,j get pass
   @ 9,j get pass1
   read
   i=lastkey()
enddo
setcolor(stdc)
if i<>27
   repl passw with pass
endif
use
sele a
restscreen(5,10,11,64,scr)
return
*
proc FirstPic
set color to /
clear
setcolor(bcol)
@ 24,0 say ''
type First.Pic
setcolor(stdc)
return
*
proc IndexSup
priv clos,c_sel
c_sel=str(select(),2)
if select('super')=0
   sele 0
   clos=.t.
else
   sele super
   clos=.f.
endif
use superlist alias super
index on ksp+tn to superpono
index on tn     to supertn
if clos
   sele super
   use
endif
sele &c_sel
return
*
proc ReadSRDs
priv tk
public all_srd, km_arr[12], mnaim_arr[12],;
       Tek_God, Tek_M_N, Tek_Mes_P, Arr_Len, Tek_Date

stor '' to Tek_God, Tek_M_N, Tek_Mes_P, all_srd
Arr_Len=12
sele 0
use srd
for i=1 to 12
   km_arr[i]    = km
   mnaim_arr[i] = naim
   skip
next
go 13
Tek_God=alltrim(naim)
do TestYear with .f.,Tek_God
use
sele 0
use supersrd
do while ! eof()
   tk=kod
   all_srd=all_srd+kod
   release dn_arr&tk, chas_arr&tk, prod_rd&tk
   public  dn_arr&tk[12], chas_arr&tk[12], prod_rd&tk
*   prod_rd&tk=timeconv(prd*100,1)/100
PROD_RD&TK=PRD
   do CreatSrd with tk
   sele 0
   use srd&tk
   for i=1 to 12
      dn_arr&tk[i]    = dn
      chas_arr&tk[i]  = chas
      skip
   next
   use
   sele supersrd
   skip
enddo
use
use pasp
if val(Pasp->tm)<1 .or. val(Pasp->tm)>12
   use
   do SetMesP
   if lastkey()=27
      return
   endif
endif
Tek_Mes_P=Pasp->tm
*       Tek_Date = ctod("01/"+Pasp->tm+"/"+Substr(Tek_God,3,2))
Tek_Date =Tek_God + Pasp->tm

Tek_M_N=mnaim_arr[val(Tek_Mes_P)]
use

EL=' '
PUBLIC nal[12],nal5[12],nal10[12]
AFILL(nal,' ')
AFILL(NAL5,' ')
AFILL(NAL10,' ')
i=1
sele 0
USE rec
do while.not.eof()
  EL=mes+str(min,19,2)+str(kl1,19,2)+str(kl_1,19,2)+str(kl_11,5,2)+;
     str(kl2,19,2)+str(kl_2,19,2)+str(kl_22,5,2)+str(kl3,19,2)+str(kl_3,19,2)+;
     str(kl_33,5,2)+str(kl4,19,2)+str(kl_4,19,2)+str(kl_44,5,2)+;
     str(kl5,19,2)+str(kl_5,19,2)+str(kl_55,5,2)+;
     str(kl6,19,2)+str(kl_6,19,2)+str(kl_66,5,2)
  nal[i]=EL
  Skip
  i=i+1
enddo
I=1
USE REC5
do while.not.eof()
  EL=mes+str(min,19,2)+str(kl1,19,2)+str(kl_1,19,2)+str(kl_11,5,2)+;
     str(kl2,19,2)+str(kl_2,19,2)+str(kl_22,5,2)+str(kl3,19,2)+str(kl_3,19,2)+;
     str(kl_33,5,2)+str(kl4,19,2)+str(kl_4,19,2)+str(kl_44,5,2)+;
     str(kl5,19,2)+str(kl_5,19,2)+str(kl_55,5,2)+;
     str(kl6,19,2)+str(kl_6,19,2)+str(kl_66,5,2)       
  nal5[i]=EL
  Skip
  i=i+1
enddo
I=1
USE REC10
do while.not.eof()
   EL=mes+str(min,19,2)+str(kl1,19,2)+str(kl_1,19,2)+str(kl_11,5,2)+;
      str(kl2,19,2)+str(kl_2,19,2)+str(kl_22,5,2)+str(kl3,19,2)+str(kl_3,19,2)+;
      str(kl_33,5,2)+str(kl4,19,2)+str(kl_4,19,2)+str(kl_44,5,2)+;
      str(kl5,19,2)+str(kl_5,19,2)+str(kl_55,5,2)+;
      str(kl6,19,2)+str(kl_6,19,2)+str(kl_66,5,2)
   nal10[i]=EL
   Skip
   i=i+1
enddo
USE
ok_or_no=.t.
return
*
proc CreatSrd
para tk
priv open_srd, sel
if ! file('srd'+tk+'.dbf')
   sel=ltrim(str(select(),2))
   if select('srd')=0
      open_srd=.t.
      sele 0
      use srd
   else
      open_srd=.f.
      sele srd
   endif
   go top
   copy next 12 to srd&tk
   if open_srd
      sele srd
      use
   endif
   sele &sel
endif
return
*
proc TestYear
para prizn,Tek_God
sele srd
go 13
if len(Tek_God)<>4 .or.prizn
   set curs on
   Tek_God=substr(Tek_God+'    ',1,4)
   setcolor(ecol)
   @ 24,0
   @ 24,1 say 'Введите год в виде ГГГГ или {Esc}-отказ';
          get Tek_God picture '9999' valid len(alltrim(Tek_God))=4
   read
   set curs off
   Tek_God=alltrim(Tek_God)
endif
if lastkey()<>27
   repl naim with Tek_God
endif
return
*
proc MySetCol
priv i,j,wrk,wrk1
declare colors[7]
colors[1]='mcolbln'
colors[2]='mcolbln1'
colors[3]='stdc'
colors[4]='mcol'
colors[5]='ecol'
colors[6]='bcol'
colors[7]='ucol'
set color to
clear
i=1
do while .t.
   for j=1 to 7
      wrk=colors[j]
      wrk1=left(&wrk+space(15),15)
      set color to
      @ j+5,3 say wrk
      setcolor(&wrk)
      @ row(),13 say wrk1 get wrk1
   next
   clear gets
   set color to
   for j=1 to 7
      @ j+5,1 prompt '>'
   next
   menu to i
   if i=0
      exit
   endif
   wrk=colors[i]
   wrk1=left(&wrk+space(15),15)
   set curs on
   @ i+5,46 get wrk1
   read
   set curs off
   if lastkey()<>27
      &wrk=trim(wrk1)
   endif
enddo
return
*
