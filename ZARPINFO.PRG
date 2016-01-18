parameter next_func
priv PressKey,MyPress
external MesPrem, KTUGet, AvanGet, TabelGet, PremGet, Ko13zGet
external AvanSay, PremFnd, AvanSeek
close data
do while .t.
 open_a_b=.f.
 sele 0
 use ssp
 set filter to z='+'
 set color to /
 clear
 setcolor(ucol)
 @ 0,0 say ' Обработка информации '
 @ 2,0 say ' '+Tek_M_N+' '+Tek_God+' '
 setcolor(stdc)
 @ 4,0 say ' Вcего      '+ltrim(str(recc(),4))+' подразд. '
 @ 5,0 say ' Закреплено '+ltrim(str(all_zak,4))+' '
 setcolor(stdc)
 imax=0
 setcolor(mcol)
 set key -1 to PressCr
 set key -2 to PressCr
 set key -5 to PressCr
 set key -6 to PressCr
 set key -7 to PressCr
 set key -8 to PressCr
 set key 084 to PressCr
 set key 116 to PressCr
 set key 083 to PressCr
 set key 115 to PressCr
 set key 022 to PressCr
 keyboard ''
 PressKey=0
 xb=0
 yb=0
 old_spr_n=SprNom
 do ListMenu with '',1,35,20,1,SprNom,SprTop,;
    'str(recno(),4)+". "+ksp+" "+nbrig+" "+d+" "+mes+" "',;
'{Esc}/{Ключ}-конец ┘-выбор F2-операт.информация F3-печать форм F8-раcчет удерж.'+;
' F6 -перевести в другое подразделение F7-переписать данные временно переведенных'+;
' Ins-создать подразделение  "s" - коррект. справочников  "t" - технол. функции ',;
    'Список структурных подразделений','',xb,yb
 set curs off
 setcolor(stdc)
 @ 24,0 clear
 set key -1 to
 set key -2 to
 set key -5 to
 set key -6 to
 set key -7 to
 set key -8 to
 set key 084 to
 set key 116 to
 set key 083 to
 set key 115 to
 set key 022 to
 if PressKey=84 .or. PressKey=116
    next_func=3
    SprNom=old_spr_n
    sele ssp
    use
    exit
 endif
 if PressKey=83 .or. PressKey=115
    next_func=2
    SprNom=old_spr_n
    sele ssp
    use
    exit
 endif
 if SprNom=0
    sele ssp
    use
    exit
 endif
 sele ssp
 go   SprTop
 skip SprNom - 1
 if z<>'+' .and. PressKey<>22
    do ErrorTxt with 'Вы ошиблись. Закрепление не было произведено !!!'
    loop
 endif
 mespod=if(PressKey=22,Tek_Mes_P,mes)
 kodpod=ksp
 namepod=nbrig
 mesname=mnaim_arr[val(mespod)]
if presskey=0.and.lastkey()=13 
    PressKey=13
 endif
 if PressKey<>0
   MyPress=PressKey
   dm=d
   if MyPress=13 .or. MyPress=-1 .or. MyPress=-7
      ok=.f.
      do MountDsk with dm,kodpod,ok,.t.
      if .not. ok
         loop
      endif
   endif
   do case
     case MyPress=13
       do ListMens with kodpod,namepod,mespod,mesname,dm
     case MyPress=-1
       do ZarpOper with kodpod,namepod,mespod,mesname,dm
     case MyPress=-2
       do ZarpOutf with kodpod,namepod,mespod,mesname,dm
     case MyPress=-5
       do VremPere with kodpod,namepod,mespod,mesname,dm
     case MyPress=-6
       do VremSobr with kodpod,namepod,mespod,mesname,dm
     case MyPress=-7
        do RecaPodr
     case MyPress=22
        setcolor(stdc)
        clear
        new_recn=0
        do NewPodr with new_recn
        if new_recn>0 .and. new_recn<=recc()
           go new_recn
           do MoveFile
           go new_recn
           if z='+'
              all_zak=all_zak+1
           endif
        endif
   endcase
   close data
   loop
 endif
enddo
close data
return
*
proc RecaPodr
close data
do OpenABC with 'ABC'
setcolor(stdc)
clear
@ 1,1 say namepod+' '+mesname+' '+Tek_God
SELE 0
USE SVO index svoindex
i=recc()
declare kodna[i], maskna[i]
do ReadMskM with kodna, maskna
sele 0
use suder index suderind
sele 0
use skn
sele basef
set filter to rpv $ '12' .and. ys $ '01'
go top
keyboard ''
end_shap=5
itog_l=20
kop_row=19
keyboard ''
do while .not. eof()
   if inkey()=27  && lastkey()=27
      exit
   endif
   setcolor(ecol)
   @ 2,1 say 'Пересчет по '+tn+'. {Esc}/{Ключ}-конец '
   setcolor(stdc)
   sele skn
   LOCA for KN=basef->NP
   sele basef
   first_ras=.t.
   @ 3,0 clear
   do ReCalc with .f.
   sele basef
   skip
enddo
sele skn
use
sele suder
use
sele svo
use
sele basef
use
sele sebaf
use
sele super
use
release kodna, maskna
return
*
proc ZarpOper
para kodpod,namepod,mespod,mesname,dm
priv noper
do OpenABC with 'ABC'
sele basef
if eof()
   sele basef
   *set rela to
   use
   sele sebaf
   use
   sele super
   use
   return
endif
noper=1
do while .t.
   setcolor(stdc)
   clear
   setcolor(bcol)
   @ 5,4,14,70 box frame+'░'
   @ 5,5 say 'Ввод оперативной информации ('+trim(namepod)+')'
   setcolor(ecol)
   @ 24,0
   @ 24,3 say '{клавиши-стрелки} - движение  {Esc}/{Ключ} - конец   ┘- выбор'
   setcolor(mcol)
   @  7,8 say ' Табель учета использования рабочего времени  '
   @  8,8 say ' Список на аванс                              '
   @  9,8 say ' Процент премии                               '
   @ 10,8 say ' Расчет премии                                '
   @ 11,8 say ' Корректировка начислений с указанным кодом   '
   @ 12,8 say ' Бригада                                      '
   @ 13,8 say ' Копия баз данных                             '
   setcolor(mcolbln)
   @  7,6 prompt '*'
   @  8,6 prompt '*'
   @  9,6 prompt '*'
   @ 10,6 prompt '*'
   @ 11,6 prompt '*'
   @ 12,6 prompt '*'
   @ 13,6 prompt '*'
   set key 27 to
   menu to noper
   set curs off
   setcolor(stdc)
   if noper=0
      exit
   endif
   @ noper+6,6 say '*'
   do case
    case noper=1
      setcolor(ecol)
      @ 24,0
      @ 24,1 say '.....Ждите.....'
      sele basef
      go top
      setcolor(stdc)
      @ 24,0
      declare allna[12]
      SELE 0
      USE SVO index svoindex
      i=recc()
      declare kodna[i], maskna[i]
      do ReadMskM with kodna, maskna
      use
      do MyDbEdit with 'Табель учета использования рабочего времени',;
         kodpod,namepod,mespod,mesname,;
         'str(rabd'+mespod+'/100,6,2)',;
         ' Таб.     Фамилия И. О.        Кол-во ',;
         ' номер                         дн/час ',;
         'TabelGet'
      sele basef
      release allna, kodna, maskna
    case noper=2
      DaNet = 1
      sele basef
      @ 24,0
      @ 24,40 say " 1 - Да / 2 - Нет "
      @ 24,1 say "   Переход на новый месяй был?" get DaNet pict "9" range 1,2
      read
      @ 24,0
      if DaNet = 1
         do MyDbEdit with 'Список на аванс',;
            kodpod,namepod,mespod,mesname,'AvanSay()',;
            ' Таб.     Фамилия И. О.         Сумма ',;
            ' номер                         аванса ',;
            'AvanGet'
      else
         do MyDbEdit with 'Список на аванс',;
            kodpod,namepod,mespod,mesname,'AwanSay()',;
            ' Таб.     Фамилия И. О.         Сумма ',;
            ' номер                         аванса ',;
            'AwanGet'
      EndIf
      sele basef
   case noper=3
      sele basef
      do MyDbEdit with 'Процент премии            ',;
         kodpod,namepod,mespod,mesname,'str(kty/100,7,3)',;
         ' Таб.     Фамилия И. О.       Процент ',;
         ' номер',;
         'KTUGet'
      sele basef

   case noper=4
       do MesPrem
    case noper=5
      setcolor(stdc)
      clear
      setcolor(ucol)
      @ 1,1 say ' Корректировка начислений с указанным кодом   '
      sele 0
      use svo
      stor '' to PremKod, PremName
      do GetKodNa with PremKod, PremName
      use
      if len(PremKod)=0
         loop
      endif
      sele basef
      rele arr_prem, i_prem, allna
      declare arr_prem[recc()],i_prem[recc()],allna[12]
      afill(arr_prem,0)
      afill(i_prem,-1)

      do MyDbEdit with trim(PremName)+' ('+PremKod+')',;
         kodpod,namepod,mespod,mesname,'PremFnd(PremName,PremKod)',;
         ' Таб.     Фамилия И. О.      Дни     Сумма   ',;
         ' номер                       часы            ',;
         'PremGet'

      rele arr_prem,i_prem,allna
      
      sele basef

*   case noper=6

*do xlmen with kodpod,namepod,mespod,mesname,dm
*retu
    case noper=7
DO copybd
   endcase
enddo
sele basef
use
sele sebaf
use
sele super
use
return
*
proc ListMens
para kodpod,namepod,mespod,mesname,dm
priv RNom,RTop
sele ssp
addrdisk=if(d=' ',0,asc(upper(left(d,1)))-asc('A')+1)
use
do OpenABC with 'ABC'
sele sebaf
lsebaf=recsize()
sele basef
lbasef=recsize()
sele super
seek kodpod
Rtop=if(eof(),-999,recno())
Rnom=1
do while .t.
   sele basef
   do MainInfP
   open_a_b=.t.
   setcolor(mcol)
   set key -1 to PressCr
   set key -2 to PressCr
   set key -3 to PressCr
   set key -4 to PressCr
   set key -5 to PressCr   && F6-Архив
   set key 22 to PressCr
   set key  7 to PressCr
   keyboard ''
   PressKey=0
   xb=0
   yb='ksp<>kodpod'
   sele super
   do ListMenu with '',1,40,21,1,RNom,RTop,'" "+tn+" "+fio+" "',;
'{Esc}/{Ключ}-конец {Ins}/{ВСТ}-создать {Del}/{УДЛ}-удалить  ┘-таб.ном. и Ф.И.О '+;
'F2-лицевой счет F3-постоянная F4-долговременная F5-расчетная информация F6-Архив',;
'Список сотрудников','',xb,yb,'TopRout'
   setcolor(stdc)
   @ 23,0 clear
   set key -1 to
   set key -2 to
   set key -3 to
   set key -4 to
   set key -5 to
   set key 22 to
   set key  7 to
   sele super
   if RNom=0
      exit
   endif
   go   RTop
   skip RNom - 1
   famio=super->fio
   sele basef
   go super->base_rec
   if PressKey=0 .and. lastkey()=13
      PressKey=13
   endif
   if PressKey<>0
      MyPress=PressKey
      do while .t.
        NextKod=0
        do case
          case MyPress=13  && изменение фамилии
             setcolor(bcol)
             @ 4,3,22,60 box frame+'░'
             @ 22,4 say 'Изменение таб.номера и Ф.И.О.'
             sele super
             old_super=recno()
             store super->fio to newfio,oldfio
             store super->tn  to newtn, oldtn
             store super->fam to newfam, oldfam
             store super->name to newnam, oldnam
             store super->ot to newot, oldot
             use superlist index supertn,superpono alias super
             setcolor(ecol)
             @ 6,4 say 'Вводите данные или нажмите Esc - отказ'
             setcolor(stdc)
             set curs on
             @ 7,5 say '"Старый" таб.номер  '+oldtn
             @ 8,5 say '"Новый"  таб.номер ' get newtn;
                    valid (if(newtn==oldtn,.t.,TestTabN(newtn,14,31));
                           .and. ! (' ' $ newtn))
             @ 10,5 say '"Старое" ФИО      '+oldfio  &&super->fio
             @ 11,5 say '"Новое"  ФИО     ' get newfio
             @ 13,5 say '"Старая" фамилия  '+oldfam  &&super->fam
             @ 14,5 say '"Новая"  фамилия ' get newfam
             @ 16,5 say '"Старое" имя      '+oldnam  &&super->name
             @ 17,5 say '"Новое"  имя     ' get newnam
             @ 19,5 say '"Старое" отч.     '+oldot  &&super->ot
             @ 20,5 say '"Новое"  отч.    ' get newot
             read
             set curs off
             sele super
             use superlist index superpono,supertn alias super
             go   old_super
             if lastkey()<>27
                repl super->fam  with newfam
                repl super->name with newnam
                repl super->ot with newot
                repl super->fio with newfio
                repl super->tn with newtn
                repl basef->tn  with newtn
             endif
          case MyPress=-1
             do ZarpLiSC with NextKod
           case MyPress=-5
              do ZarpArch with NextKod
          case MyPress=-2
             do ZarpPost with NextKod
           case MyPress=-3
              do ZarpDolg with NextKod
          case MyPress=-4
             do ZarpCoRa with NextKod
          case MyPress=22
            sele super
            old_super=recno()
            use superlist index supertn,superpono alias super
            sele basef
            do while int(diskspace(addrdisk)/(lsebaf*12+lbasef))>=1
               sele basef
               do MainInfP
               setcolor(bcol)
               @ 12,5,19,50 box frame+'░'
               @ 12,6 say 'Новый работник подразделения '+trim(namepod)
               newtn=space(len(tn))
               newfio=space(len(super->fio))
               setcolor(ecol)
               @ 14,7 say 'Вводите данные или нажмите Esc - отказ'
               setcolor(stdc)
               set curs on
               @ 16,7 say 'Табельный номер ' get newtn;
                       valid (TestTabN(newtn,16,30)) .and.;
                             .not.(' ' $ newtn)
               @ 17,7 say 'Ф И О           ' get newfio
               read
               set curs off
               if lastkey()=27
                 exit
               endif
               setcolor(ecol)
               @ 24,0
               @ 24,1 say '.....Ждите.....'
               sele basef
               loca for tn=replicate('*',len(tn))
               if eof()
                  append blank
                  sele super
                  append blank
               else
                  sele super
                  seek replicate('*',len(tn))
                  if eof()
                     append blank
                  endif
                  sele basef
                  j=fcount()
                  for i=1 to j
                    wrk=field(i)
                    do case
                       case type('&wrk')='C'
                          repl &wrk with ' '
                       case type('&wrk')='N'
                          repl &wrk with 0
                    endcase
                  next
                  i=recc()*12
                  sele sebaf
                  do while recc()<i
                     append blank
                  enddo
                  sele basef
                  i=(recno()-1)*12+1
                  sele sebaf
                  go i
                  for j=0 to 11
                     do BlankRec with .f.
                     skip
                  next
              endif
              sele basef
              repl tn   with newtn, ys  with '0', pts with '1',;
                   kpss with '0', chp with '1',   rpv with '1',;
                   np   with '0', kty with 100
              b_r=recno()
              i=recc()*12
              go top
              go bott
              sele super
              repl ksp with kodpod,tn with newtn,fio with newfio,;
                   rpv with basef->rpv,base_rec with b_r
              sele sebaf
              do while recc()<i
                 append blank
              enddo
              go top
              go bott
              setcolor(ucol)
              @ 21,17 say newtn+'. '+newfio+' добавлен'
              setcolor(stdc)
              @ 24,0
            enddo
            sele super
            use
            setcolor(ecol)
            @ 24,0
            @ 24,1 say '.....Ждите.....'
            use superlist index superpono,supertn alias super
            seek kodpod
            RTop=if(eof(),-999,recno())
            RNom=1
            exit
          case MyPress=7
            r_del=' '
            do AskYesNo with r_del,24,;
               trim(super->tn+' '+super->fio)+': Удалить cотрудника ? ',;
               ecol
            if r_del<>'D'
               exit
            endif
            sele super
            repl tn  with replicate('*',len(tn)),;
                 fio with ' -свободен- ',;
                 ksp with replicate('*',len(ksp)),;
                 rpv with ' '
            sele basef
            go super->base_rec
            repl tn with replicate('*',len(tn)), ys with '*'
            sele super
            seek kodpod
            Rtop=if(eof(),-999,recno())
            Rnom=1
            exit
          other
            exit
        endcase
        MyPress=NextKod
      enddo
   endif
enddo
sele sebaf
use
sele basef
set rela to
use
sele super
use
return
*
function TestTabN
parameters newtabn,r,c
priv i,obl,wrk
obl=str(select())
sele super
i=22
seek newtabn
if found()
   wrk=savescreen(r,c,r+1,79)
   setcolor(ecol)
   @ r,c say '<--занят. {Esc}/{Ключ}-отказ, {Ins}/{ВСТ}-дубль'
   @ r+1,c+20 say super->fio
   keyboard ''
   i=0
   do while (i<>22 .and. i<>27)
      i=inkey(0)
   enddo
   setcolor(stdc)
   restscreen(r,c,r+1,79,wrk)
endif
sele &obl
return (if(i=22,.t.,.f.))
*
proc MyDbEdit
para kind,kodpod,namepod,mespod,mesname,prom_exp,shap1,shap2,get_proc
clear
sele super
seek kodpod
if eof()
   set filter to
   return
endif
top_r=recno()
fst_say=.t.
i_row=1
do while .t.
   setcolor(stdc)
   if fst_say
      clear
      @ 0,0 say trim(name_pred)+' '+kodpod+'. '+;
                namepod+' '+mesname+' '+Tek_God
      @ 1,1 say kind
      @ 2,0,5,max(len(shap1),len(shap2))+1 box frame+' '
      @ 3,1 say shap1
      @ 4,1 say shap2
   endif
   stor 6 to i, ed_start
   sele super
   go top_r
   do while .not. eof() .and. ksp==kodpod .and. i<24
      sele basef
      go super->base_rec
      if ys='0'
         setcolor(stdc)
         @ i,1 say ' '+super->tn+' '+super->fio
         setcolor(mcolbln)
         @ i,col()+1 prompt '*'
         c_get=col()+1
         setcolor(stdc)
         @ i,c_get say &prom_exp
         i=i+1
      endif
      sele super
      skip
   enddo
   setcolor(ecol)
   @ 24,0
   @ 24,1 say chr(24)+chr(25)+'{PgUp}{PgDn}-движение  {Esc}/{Ключ}-конец  ┘-заполнение поля'
   setcolor(mcolbln)
   fst_say=.f.
   keyboard ''
   menu to i_row
   sele super
   go top_r
   do case
      case i_row=0
        exit
      case lastkey()=18
        skip ed_start-23
        if bof() .or. ksp<>kodpod
          seek kodpod
        endif
        top_r=recno()
        fst_say=.t.
      case lastkey()=3
        skip 23-ed_start
        if eof() .or. ksp<>kodpod
          go top_r
        endif
        top_r=recno()
        fst_say=.t.
      case lastkey()=13
        i=1
        do while i<=i_row
           sele basef
           go super->base_rec
           if ys='0'
              i=i+1
           endif
           sele super
           skip
        enddo
        setcolor(ecol)
        @ 24,0
        set curs on
        sele basef
        do &get_proc
        set curs off
   endcase
enddo
return
*
proc KTUGet
@ 24,1 say 'Вводите процент премии'
setcolor(stdc)
i=kty/100
@ ed_start-1+i_row,c_get get i picture '###.###'
read
if lastkey()<>27
   repl kty with i*100
endif
return
*
func AvanSay
priv j,j1
j=AvanSeek('915')
j1=ltrim(str(j,2))
return if(j=0,'---***---',str(sku&j1/100,11,2))
*
func AwanSay
return str(pa/100,11,2)
*
func AvanSeek
para kodavan
priv j,j1
if ku1=kodavan
   j=1
else
   wrk='*'+ku1+'*'+ku2+'*'+ku3+'*'+ku4+'*'+ku5+'*'+ku6+;
       '*'+ku7+'*'+ku8+'*'+ku9+'*'+ku10+'*'+ku11+'*'+ku12
   j=at('*'+kodavan,wrk)
   if j=0
      j=at('*   ',wrk)
   endif
endif
return if(j=0,j,int(j/(len(ku1)+1))+1)
*
proc AvanGet
priv i,j,wrk
@ 24,1 say 'Вводите cумму аванcа'
setcolor(stdc)
j=AvanSeek('915')
if j=0
   return
endif
j=ltrim(str(j,2))
i=sku&j/100
@ ed_start-1+i_row,c_get get i picture '########.##'
read
if lastkey()<>27
   repl ku&j with '915', kmu&j with mespod, sku&j with i*100
endif
return
*
proc AwanGet
priv i,j,wrk
@ 24,1 say 'Вводите cумму аванcа'
setcolor(stdc)
i=pa/100
@ ed_start-1+i_row,c_get get i picture '########.##'
read
if lastkey()<>27
   repl pa with i*100
endif
return
*
proc TabelGet
priv w,w1,i,j
@ 24,1 say 'Вводите количество отработанных дней/часов'
setcolor(stdc)
*i=timeconv(rabd&mespod,2)/100 &&arr_tab[recno()]
i=rabd&mespod/100
@ ed_start-1+i_row,c_get get    i picture '###.##'
read
if lastkey()<>27
   w1='10'+basef->pts
   if .not. ('*'+w1 $ '*'+vo1+'*'+vo2+'*'+vo3+'*'+vo4+'*'+vo5+'*'+vo6+;
                      '*'+vo7+'*'+vo8+'*'+vo9+'*'+vo10+'*'+vo11+'*'+vo12)
      j=AddNewK(w1,w1,0,.t.)
   endif
   repl rabd&mespod with i*100
sele d
use skl
locate for basef->tn=tn
if found()
replace ch with i
endif
sele basef
   do NachVRO with i,i
endif
return
*
func PremFnd
para PremName,PremKod,mesfind
priv i,prom_txt,wrk
if type('mesfind')<>'C'
   mesfind=mespod
endif
sele basef
i=i_prem[recno()]
if i=-1
   wrk='*'+vo1+kmn1+'*'+vo2+kmn2+'*'+vo3+kmn3+'*'+vo4+kmn4+;
       '*'+vo5+kmn5+'*'+vo6+kmn6+'*'+vo7+kmn7+'*'+vo8+kmn8+;
       '*'+vo9+kmn9+'*'+vo10+kmn10+'*'+vo11+kmn11+'*'+vo12+kmn12
   i=at('*'+PremKod+mesfind,wrk)
   if i=0
      i=at('*'+space(len(PremKod+mesfind)),wrk) &&AddNewK(PremKod,PremKod,0,.t.)   && ,.f.)
   endif
   if i<>0
      i=int(i/(len(vo1)+len(kmn1)+1))+1
   endif
   sele basef
   if i>0 .and. i<13
      i_prem[recno()]=i
      wrk=ltrim(str(i,2))
      arr_prem[recno()]=svon&wrk/100
   else
      stor 0 to i_prem[recno()], i
   endif
   if noper=4
      sum_prem=sum_prem+arr_prem[recno()]
   endif
endif
if i<=0
   prom_txt='<<<<<<<нет места в лицевом счете>>>>>>>'
else
   i=ltrim(str(i,2))
   prom_txt=if(noper=4,'',str(dn&i/100,6))+' '+;
                       str(arr_prem[recno()],9,2)
endif
return prom_txt
*
proc PremGet
priv i,j,j1,i1,nu,isu,idni
j=i_prem[recno()]
if j<=0
   return
endif
sele basef
nu=ltrim(str(j))
setcolor(ecol)
@ 23,0 clear
@ 23,1 say 'Вводите: 1) кол.дней или часы.минуты;'
@ 24,1 say '2) cумму начиcленного или 0-для автоматич.расчета '+;
           'или процент (например: 30% ) ' &&со знаком минус'
setcolor(stdc)
isu =str(arr_prem[recno()],9,2)
isu='         '
idni=dn&nu/100
plus=if((val(vo&nu)>=201.and.val(vo&nu)<=210),5,0)
set curs on
@ ed_start-1+i_row,c_get  get idni    picture '###.##'
                         *valid ( testdni() )
@ ed_start-1+i_row,col()+1 get isu;
        valid val(if('%' $ isu,left(isu,at('%',isu)-1),isu))<1000000000
read
setcolor(stdc)
@ 23,0 clear

if lastkey()<>27
   repl svon&nu with val(if('%' $ isu,left(isu,at('%',isu)-1),isu))*100,;
        dn&nu with idni*100,kmn&nu with mespod,;
        vo&nu with PremKod
sele d
use skl
locate for basef->tn=tn
if found()
replace ch with idni
endif
sele basef

   if ((val(isu)=0 .or. ('%' $ isu)) .and. pts $ '123')
      do NachInt with val(nu)
   endif
   arr_prem[recno()]=svon&nu/100
endif
return
*
func TestDni
priv tk
tk=TestKale()
return     (    if(pts='2'.and.plus=0,;
                   ( abs(idni-int(idni))<0.61 .and.;
                    idni<=chas_arr&tk[val(kmn&nu)] );
                   ,;
                   ( int(idni)=idni .and. ;
                     idni<=dn_arr&tk[val(kmn&nu)]+plus);
                  );
           )
*
proc GetKodNa
para PremKod, PremName
SvoNom=1
SvoTop=if(eof(),-999,recno())
xb=0
yb=0
do ListMenu with '',1,47,22,1,SvoNom,SvoTop,;
                 '" "+kvoy+" "+naim+" "',;
   ' {Esc}/{Ключ} - конец  ┘ - выбор ','Укажите начисление','',xb,yb
sele svo
if SvoNom=0
   return
endif
PremKod=kvoy
PremName=naim
setcolor(ecol)
@ 24,0
@ 24,1 say '.....Ждите.....'
return
*
proc MainInfP
set color to /
clear
setcolor(ucol)
@ 0,0 say ' '+kodpod+'. '+trim(namepod)+' '
@ 1,0 say ' '+mesname+' '+Tek_God+' '
setcolor(stdc)
@ 4,1 say 'Вcего записей в базе данных '+ltrim(str(recc()))
@ 5,1 say '      (включая удаленные)'
@ 6,1 say 'Можно добавить              '+;
          ltrim(str(int(diskspace(addrdisk)/(lsebaf*12+lbasef))))
return
*
proc SayNaUd
para nach,sr,nu
setcolor(stdc)
if nach
   @ sr,2  SAY kmn&nu
   @ sr,6  say vo&nu
   @ sr,29 say if(dn&nu=0,'      ',str(dn&nu/100,6,2))
   @ sr,35 say if(svon&nu=0,'         ',str(svon&nu/100,10,2))
   IF vo&nu<>' '
     SELE svo
     seek basef->vo&nu
     IF EOF()
        @ sr,11 SAY 'Kод неверен '
     ELSE
        @ sr,11 SAY NAIM
     ENDIF
   ENDI
else
   @ sr,46 say kmu&nu
   @ sr,49 say ku&nu
   IF ku&nu<>' '
     SELE suder
     seek basef->ku&nu
     IF EOF()
        @ sr,54 SAY 'Kод неверен '
     ELSE
        @ sr,54 SAY NAIM
     ENDIF
   ENDI
   @ sr,69 say if(basef->sku&nu=0,'         ',str(basef->sku&nu/100,11,2))
endif
sele basef
return
*
proc itogNU
para ln,repl_or_no,say_or_no
priv nac,ude,dolg,idolg,r_o_n,s_o_n
if pcount()<2
   r_o_n=.f.
else
   r_o_n=repl_or_no
endif
if pcount()<3
   s_o_n=.t.
else
   s_o_n=say_or_no
endif
nac=kpm+svon1+svon2+svon3+svon4+svon5+svon6+svon7+svon8+svon9+svon10+svon11+svon12
ude=sku1+sku2+sku3+sku4+sku5+sku6+sku7+sku8+sku9+sku10+sku11+sku12
dolg=nac-ude
*idolg=if(ys='0',int(dolg/100),dolg)
idolg=(dolg/100)

if s_o_n
   @ ln,0 clear to ln+1,79
   @ ln,11 say 'Итого начислено '
   @ ln,34 say str(nac/100,12,2)
   @ ln,54 say 'Итого удержано  '
   @ ln,69 say str(ude/100,11,2)
   if dolg<0
      setcolor(ucol)
      @ ln+1,1 say 'Долг за работ.'+str(-dolg/100,12,2)
   else
      setcolor(stdc)
      @ ln+1,1 say 'Остат.'+str(dolg/100 - idolg,6,2)+;
                   '.К выдаче'+str(idolg,12,2)
   endif
endif
if r_o_n
   if dolg<0
      repl oz with 0, cd with -dolg
   else
      repl oz with dolg-idolg*100, cd with 0
   endif
endif
setcolor(stdc)
return
*
proc DnUpSotr
priv old_r
setcolor(stdc)
@ 1,60
do case
   case PressKey=18
       sele super  &&basef
       old_r=recno()
       skip -1
       if bof() .or. ksp<>kodpod
          go old_r
          setcolor(ecol)
          @ 1,60 say '*начало*'
          setcolor(stdc)
       else
          first_lst=.t.
          first_ras=.t.
       endif
       sele basef
       go super->base_rec
   case PressKey=3
       sele super &&basef
       old_r=recno()
       skip
       if eof() .or. ksp<>kodpod
          go old_r
          setcolor(ecol)
          @ 1,60 say '*конец *'
          setcolor(stdc)
       else
          first_lst=.t.
          first_ras=.t.
       endif
       sele basef
       go super->base_rec
endcase
return
Proc NachVro
PARA wrk_dn,CH
tk=TestKale() && if(type('basef->kalendar')<>'C','',trim(basef->kalendar))
**********
* MESPOD  C(2)  Расчетный месяц
* DN_ARR[i] массив из 12-ти элементов,каждый N(2)
* DD      N     Отработано дней
* CH      N     Отработано часов
* KODN    C(3)  Kод начисления
**********,KODN
PRIV KM,MYPR,A,VRO,I,J,II,JJ,S
KM=MESPOD

DO CASE
 CASE PTS='1'
  A=DN_ARR&tk[VAL(mespod)]
 CASE PTS='2'
  a=100
 case PTS='3'
  A=1
 othe
  return
ENDC

II=0
DO WHIL II<7
 II=II+1
 I=STR(II,1)
 if val(VO&I)<>0 .and. kmn&i=mespod
  JJ=II-1
  J=STR(JJ,1)
  val_voi=val(vo&i)
  DO CASE
   CASE val_voi>=301 .and. val_voi<=320
  Oklad = OKS
   IF mPen > 0
   Oklad = mPen
  endif
    repl svon&i with TestSvon(round((Oklad*wrk_dn*nsu&j/a)/10000,0)),;
         dn&i   with wrk_dn*100
   CASE val_voi>=331 .and. val_voi<=340
    repl svon&i with TestSvon(round(wrk_dn*nsu&j/a,0)),;
         dn&i   with wrk_dn*100
   CASE VO&I='10'+pts
    repl svon&i with TestSvon(round(OKS*wrk_dn/a,0)),;
         dn&i   with wrk_dn*100
   CASE Val_voi=395
  Oklad = OKS
   IF mPen > 0
   Oklad = mPen
  endif
  REPL SVON&I WITH TestSvon(round((Oklad*wrk_dn*nsu&j/A)/10000,0)),dn&i with wrk_dn*100
  ENDC
 endi
ENDD
do SeveRaio
RETU
*
proc SeveRaio
priv iiw, ii1, ii2, wrk
 
 * -------СЕВЕРНЫЕ------
   iiw=at('*366','*'+nvu1+'*'+nvu2+'*'+nvu3+'*'+nvu4+'*'+nvu5+'*'+nvu6)
   if iiw<>0
      first_lst=.t.
      ii1=str(int((iiw/4)+1),1)
      ii2=AddNewK('366','366',;
                  MIN ( DOPS_MAXS , PrSr(21) ) * nsu&ii1 / 10000,.t.)
      if ii2<>0
         wrk = ltrim(str(ii2,2))
         repl dn&wrk with rabd&mespod
      endif
   endif
 * ---------------------
 
 * -------РАЙОННЫЕ-------
   if dops_rk<>0
      first_lst=.t.
      do RaiKoeff
   endif
 * ----------------------

return
*
proc RaiKoeff
priv itsvo, itsvo1, s1, s2, i
itsvo1=PRSR(17)
itsvo =PRSR(18)
IF ITSVO1+ITSVO>DOPS_MAXR
  S1=MIN(ITSVO1,DOPS_MAXR)
  S2=MAX(0,DOPS_MAXR-S1)
else
  s1=itsvo1
  s2=itsvo
ENDIF
S1=S1*(DOPS_RK-1)
S2=S2*(DOPS_RK-1)
do ReplRaiT with AddNewK('361','361',s1,.t.)
do ReplRaiT with AddNewK('362','362',s2,.t.)
return
*
proc ReplRaiT
para ln_r
priv wrk
if ln_r<>0
   wrk=ltrim(str(ln_r,2))
   repl dn&wrk with rabd&mespod
endif
return
