parameter kodpod,namepod,mespod,mesname,dm
close data
pech=1
r_pech=1
tp_out=0
dm_outf   = dm
kod_outf  = kodpod
name_outf = namepod
mes_outf  = mespod
do while .t.
   setcolor(stdc)
   clear
   dm      = dm_outf
   kodpod  = kod_outf
   namepod = name_outf
   mespod  = mes_outf
   setcolor(bcol)
   @ 10,5,19,65 box framedbl+'▒'
   @ 10,6 say 'Печать выходных форм ('+trim(namepod)+')'
   setcolor(stdc)
   @ 12,10 say 'Вы можете распечатать:'
   setcolor(ecol)
   @ 24,0
   @ 24,3 say;
  '{клавиши-стрелки} - движение  {Esc}/{Ключ} - конец работы  ┘/{Ввод} - выбор'
   setcolor(mcol)
   @ 14,15 say ' Выходные формы по подразделению        '
   @ 15,15 say ' Выходные формы по организации в целом  '
   @ 16,15 say ' Печать платежных поручений             '
   setcolor(mcolbln)
   @ 14,13 prompt '*'
   @ 15,13 prompt '*'
   @ 16,13 prompt '*'
   set key 27 to
   menu to pech
   set curs off
   setcolor(stdc)
   @ 24,0
   if pech=0
      exit
   endif
   @ pech+13,13 say '*'
   do case
    case pech=1
      do PodrForm
    case pech=2
      do OrgaForm
    case pech=3
      do OutfPlPr
   endcase
   close data
enddo
return
*
proc PodrForm
priv pf
pf=1
do while pf<>0
   set color to /
   clear
   setcolor(bcol)
   @ 4,15,if(nar_mpo,20,20),65 box frame+' '
   @ 4,16 say 'Формы по подразделению '+trim(kodpod)+'-'+trim(namepod)
   setcolor(mcol)
   @  5,17 say ' Платежная ведомость на выдачу аванса           '
   @  6,17 say ' Платежная ведомость на выдачу зарплаты         '
   @  7,17 say ' Расчетная ведомость по зарплате                '
   @  8,17 say ' Расчетные листки по подразделению              '
   @  9,17 say ' Расчетный листок (выборочно по сотрудникам)    '
   @ 10,17 say ' Ведомость начислений и удержаний з/п           '
   @ 11,17 say ' Обнуление копеек /рассчет/                     '
   @ 12,17 say ' Ввод пособий и компенсаций/справочник/         '
   @ 13,17 say ' Расчет индексации                              '
   @ 14,17 say ' Ведомость удержания  профсоюзных   взносов     '
   @ 15,17 say ' Ведомость удержания  взносов в пенсионный фонд '
   @ 16,17 say ' Сведения о заpаботной плате                    '
   @ 17,17 say ' Справочник работающих                          '
   @ 18,17 say ' Ввод прочих начислений и удержаний /справочник/'
   @ 19,17 say ' Запись прочих в базы                           '
   
   setcolor(ecol)
   @ 24,0
   @ 24,3 say;
     '{клавиши-стрелки} - движение  {Esc}/{Ключ} - конец работы  ┘ - выбор'
   setcolor(mcolbln)
   @  5,16 prompt ''
   @  6,16 prompt ''
   @  7,16 prompt ''
   @  8,16 prompt ''
   @  9,16 prompt ''
   @ 10,16 prompt ''
   @ 11,16 prompt ''
   @ 12,16 prompt ''
   @ 13,16 prompt ''
   @ 14,16 prompt ''
   @ 15,16 prompt ''
   @ 16,16 prompt ''
   @ 17,16 prompt ''
   @ 18,16 prompt ''
   @ 19,16 prompt ''
   menu to pf
   @ 24,0
   if pf=0
      exit
   endif
   setcolor(ecol)
   @ pf+4,16 say '*'
   setcolor(stdc)
   ok=.f.
   do MountDsk with dm,kodpod,ok,.t.
   if .not. ok
      loop
   endif
   do OpenABC with if(pf=15,'ABC','AB')

   do while .t.
      Page_Size=66
      Page_Nom=1
      tp_out=KindOutF(tp_out)
      if tp_out=0
         exit
      endif
      do case
         case pf=1
            do OutfPLAZ with name_pred,kodpod+' '+namepod,;
                             'A',mesname+' '+Tek_God
         case pf=2
            do OutfPLAZ with name_pred,kodpod+' '+namepod,;
                             'Z',mesname+' '+Tek_God
         case pf=3
            do RascVedo
         case pf=4
            do PdRscLst with .f.
         case pf=5
            do PdRscLst with .t.
         case pf=6
             do OutfVeNa with 'P'
         case pf=7
do obnk
*         case pf=8
*    do posmen
*         case pf=9
*do in
*         case pf=10
*             do OutfVeVz with 'PR'
*         case pf=11
*             do OutfVeVz with 'PT'
*         case pf=12
*    do s_zpl
         case pf=13
  do sprrab
         case pf=14
do pro
         case pf=15
do pro1
         case pf=16
*             do Outf13zp
*        case pf=17
*            macronar='vrzp'
*            do &macronar
      endcase
      SET PRINT OFF
      set cons on
      do case
         case tp_out=1 .and. prow()>1
            eject
         case tp_out=2
            do ErrorTxt with 'Конец формы...'
      endcase
      
      OTV=' '
      do AskYesNo with otv,24,' Повторить ? ',ecol
      
      if otv<>'D'.or.lastkey()=27
         exit
      endif
      @ 24,0
   enddo
   close data
enddo
return
proc RascVedo
priv kop,ost,dolg,kolrab,i
stor 0 to kop,ost,dolg,kolrab
close data
do OpenABC with 'A',"000"
sele 0
use svo index svoindex
lena=recc()
declare kodna[lena], sumna[lena], dnei[lena]
afill(sumna,0)
afill(dnei,0)
do ReadMskM with kodna
sele 0
use suder index suderind
leyd=recc()
declare kodyd[leyd], sumyd[leyd]
afill(sumyd,0)
do ReadMskM with kodyd
do FRscVedo with kolrab,kop,ost,dolg,kodna,sumna,dnei,lena,kodyd,sumyd,leyd,''
do ORscVedo with kolrab,kop,ost,dolg,kodna,sumna,dnei,lena,kodyd,sumyd,leyd,;
   '           РАСЧЕТHАЯ ВЕДОМОСТЬ ПО ЗАРАБОТHОЙ ПЛАТЕ (по подразделению) ',;
   namepod,kodpod
close data
return
*
proc FRscVedo
para kolrab,kop,ost,dolg,kodna,sumna,dnei,lena,kodyd,sumyd,leyd,nakoproc
priv i,j
setcolor(ecol)
@ 24,0
@ 24,1 SAY kodpod+'.'+namepod+'.....Ж д и т е.....'
sele basef
kolrab=0
SET FILTER TO RPV $ '12' .AND. YS $ '01'
GO TOP
DO WHILE .NOT.EOF()
   setcolor(ucol)
   @ 24,70 say tn
   for i=1 to 12
     nu=if(i<10,str(i,1),str(i,2))
     if vo&nu<>' '
        j=ascan(kodna,vo&nu)
        if j>0 .and. j<=lena
          sumna[j]=sumna[j]+svon&nu
          dnei[j] =dnei[j] +dn&nu
        else
          setcolor(ecol)
          @ 24,0 say 'Код начисления '+vo&nu+;
                     ' не будет учтен. Внеcите его в cправочник'
          inkey(5)
        endif
     endif
     if ku&nu<>' '
        j=ascan(kodyd,ku&nu)
        if j>0 .and. j<=lena
          sumyd[j]=sumyd[j]+sku&nu
        else
          setcolor(ecol)
          @ 24,0 say 'Код удержания '+ku&nu+;
                     ' не будет учтен. Внеcите его в cправочник'
          inkey(5)
        endif
     endif
   next
   sele basef
   kop   = kop  + kpm
   ost   = ost  + oz
   dolg  = dolg + cd
   kolrab= kolrab + 1
   if len(nakoproc)<>0
      do &nakoproc
   endif
   skip
ENDDO
setcolor(stdc)
return
*
proc ORscVedo
para kolrab,kop,ost,dolg,kodna,sumna,dnei,lena,kodyd,sumyd,leyd,fnaim,;
     n_p,k_p
priv i,j,a,pri,i1,i2,iw,o,o1
o='   '
o1='  '
do ShapRscV
summan=0
for i=1 to lena
  summan=summan+sumna[i]
next
summau=0
for i=1 to leyd
  summau=summau+sumyd[i]
next
maxnu=max(lena,leyd)
i1=0
i2=1
priz=.t.
do while priz
   outl=space(45)+'|'
   priz=.f.
   if i1<=lena
      if i1=0
         outl='                Копейки прош.мес.  '+STR(kop/100,9,2)+' |'
         priz=.t.
      else
         do while i1<=lena
            if sumna[i1]<>0
               exit
            endif
            i1=i1+1
         enddo
         if i1<=lena
            wrk=kodna[i1]
            sele svo
            seek wrk
            outl=STR(timeconv(dnei[i1],1)/100,11,2)+'  '+wrk+' '+naim+;
                 ' '+STR(sumna[i1]/100,11,2)+'|'
            priz=.t.
         endif
      endif
      i1=i1+1
   endif
   if i2<=leyd
      do while i2<=leyd
         if sumyd[i2]<>0
            exit
         endif
         i2=i2+1
      enddo
      if i2<=leyd
         wrk=kodyd[i2]
         sele suder
         seek wrk
         outl=outl+' '+wrk+'  '+left(naim,10)+' '+STR(sumyd[i2]/100,15,2)
         priz=.t.
      endif
      i2=i2+1
   endif
   if priz
      if prow()>Page_Size .or. row()>Page_Size
         do EndPage with 'ShapRscV'
      endif
      ? outl
   endif
enddo
if prow()>Page_Size .or. row()>Page_Size
  do EndPage with 'ShapRscV'


endif
? ' '
? ' Итого начислено            '+STR((SUMMAN+KOP)/100,15,2)+;
  '  | Итого удержано  '+STR(SUMMAU/100,15,2)
                   A=KOP+SUMMAN-SUMMAU-OST + dolg
  *   A=KOP+SUMMAN-SUMMAU-OST
IF A<0
 ? ''
ELSE
 if prow()>Page_Size .or. row()>Page_Size
    do EndPage with 'ShapRscV'
 endif
 ? ' Остаток                     '+STR(ost/100,15,2)+;
   ' | К выдаче       '+STR(A/100,15,2)
ENDIF
if prow()>Page_Size .or. row()>Page_Size
   do EndPage with 'ShapRscV'
endif
? '                                             | Долг       '+;
  STR(DOLG/100,19,2)
do DaDoPo with 'ShapRscV'
sele svo
use
sele suder
use
return
*
proc ShapRscV
@ 0,0 say ''
if Page_Nom=1
  ? name_pred
  ? fnaim
  ? '                          ЗА '+mesname+' '+Tek_God+' по '+alltrim(str(kolrab))+' работающим'
endif
? substr(k_p+' '+n_p+space(67),1,67)+'Лист '+;
  substr(str(100+Page_Nom,3),2,2)+'-'+mespod
type rscv.sha
return
*
proc PdRscLst
para list_sotr
priv rrr
****************************
SELE 0
USE DOLGPR
index on TN+Left(dtos(Data),6) to DolgPrT

SELE 0
USE VEDARX
index on Tn+dtos(data) to VedArxT

SELE 0
USE SVO index svoindex
SELE 0
USE SUDER index suderind
sele super
set filter to rpv $ '12'
seek kodpod
if list_sotr
   VremTop=if(eof(),-999,recno())
   VremNom=1
   do while .t.
      xb=0
      yb='ksp<>kodpod'
      keyboard ''
      sele super
      do ListMenu with '',1,41,22,1,VremNom,VremTop,;
         '" "+tn+" "+fio+" "',;
         ' {Esc}/{Kлюч} - отказ  ┘/{Ввод} - выбор (печать расчетного листка)',;
         'Список сотрудников','',xb,yb,'TopRout'
      if VremNom=0
         exit
      endif
      sele basef
      go super->base_rec
      do OutfRali
      do ErrorTxt with ' ... '
   enddo
else
   sele super
   do while .not. eof() .and. ksp=kodpod
      sele basef
      go super->base_rec
      if .not.(ys $ '01')
         sele super
         skip
         loop
      endif
      rrr=' '
      if tp_out=2
         do AskYesNo with rrr,24,trim(super->fio)+;
          ': печатать расч. листок ? ({ESC}-прекратить) ',ecol
         if lastkey()=27
            exit
         endif
      else
         rrr='D'
      endif
      if rrr='D'
         do OutfRaLi
      endif
      @ 0,0 say ''
      sele super
      skip
   enddo
endif
set filter to
sele svo
use
sele suder
use
**************************
Sele DolgPr
use
erase DolgPrT.Ntx

Sele VedArx
use
erase VedArxT.Ntx
return
*
func KindOutF
para tp_p
priv tp_out,Scr
Scr=savescreen(12,8,21,59)
Page_Size=60
tp_out=if(tp_p=2,3,if(r_pech=1,1,2))
do while .t.
   setcolor(stdc)
   @ 12,8,21,59 box frame+'░'
   setcolor(bcol)
   @ 14,12 say ' Укажите режим вывода формы '
   @ 15,12 say ' или нажмите {Esc} - отказ  '
   setcolor(mcol)
   @ 17,12 say ' Печать с паузой для установки листа бумаги '
   @ 18,12 say ' Печать без паузы                           '
   @ 19,12 say ' Вывод на дисплей                           '
   setcolor(mcolbln)
   @ 17,10 prompt '*'
   @ 18,10 prompt '*'
   @ 19,10 prompt '*'
   keyboard ''
   menu to tp_out
   setcolor(bcol)
   @ 17,10,19,10 box frame+'░'
   if tp_out=0
      exit
   endif
   setcolor(ecol)
   @ tp_out+16,10 say '*'
   setcolor(bcol)
   r_pech=0
   do case
      case tp_out=1 .or. tp_out=2
        if tp_out=1
           r_pech=1
        endif
        tp_out=1
        @ 14,10,20,58 box replicate('░',9)
        @ 15,12 say 'Укажите количество строк   '
        @ 16,12 say 'на странице или {Esc}-отказ'
        set curs on
        @ 18,16 get Page_Size picture '##' valid Page_Size>0
        read
        set curs off
        setcolor(stdc)
        if lastkey()=27
           tp_out=0
           exit
        endif
        SET PRINT ON
        set cons off
        exit
      case tp_out=3
        tp_out=2
        Page_Size=22
        setcolor(stdc)
        clear
        exit
   endcase
enddo
if tp_out<>0
   setcolor(ecol)
   @ 24,0
   @ 24,1 SAY '.....В ы в о д  ф о р м ы  н а  '+;
             if(tp_out=1,'п е ч а т ь','д и с п л е й')+'.....Ж д и т е.....'
   setcolor(stdc)
   @ 0,0 say ''
   setprc(0,0)
endif
if tp_out<>2
   restscreen(12,8,21,59,Scr)
endif
return tp_out
*
proc EndShap
para kol
? '                           ЗА '+Tek_M_N+' '+Tek_God
if kol>1
   ? space(int((70-len(trim(sorg->naim)))/2))+trim(SORG->NAIM)
endif
if kol>2
   ? '                        РАСЧЕТHЫЙ СЧЕТ '+SORG->RS
   ? '                        В '+sorg->ob
endif
return
*
proc Zaversh
para shaproc,it2
V_TOGG=0
 ? ''
 if prow()>=Page_Size .or. row()>=Page_Size
   do EndPage with shaproc
 endif
 ? ' СУММA ПРОПИСЬЮ: '

V_TOGG=IT2/100
    do prop with V_TOGG
 do DaDoPo with shaproc
 
return
*
proc RascVedo1
priv kop,ost,dolg,kolrab,i
stor 0 to kop,ost,dolg,kolrab
close data
do OpenABC with 'A',"000"
sele 0
use svo index svoindex
lena=recc()
declare kodna[lena], sumna[lena], dnei[lena]
afill(sumna,0)
afill(dnei,0)
do ReadMskM with kodna
sele 0
use suder index suderind
leyd=recc()
declare kodyd[leyd], sumyd[leyd]
afill(sumyd,0)
do ReadMskM with kodyd
do FRscVedo with kolrab,kop,ost,dolg,kodna,sumna,dnei,lena,kodyd,sumyd,leyd,''
do ORscVedo1 with kolrab,kop,ost,dolg,kodna,sumna,dnei,lena,kodyd,sumyd,leyd,;
   '           РАСЧЕТHАЯ ВЕДОМОСТЬ ПО ЗАРАБОТHОЙ ПЛАТЕ (по подразделению) ',;
   namepod,kodpod
close data
return
proc ORscVedo1
para kolrab,kop,ost,dolg,kodna,sumna,dnei,lena,kodyd,sumyd,leyd,fnaim,;
     n_p,k_p
priv i,j,a,pri,i1,i2,iw,o,o1,O2,O3,O4,O5,O6,o7
STORE ' 'TO o,o1,O2,O3,O4,O5,O6,o7
A=0
summan=0
for i=1 to lena
  summan=summan+sumna[i]
next
summau=0
for i=1 to leyd
  summau=summau+sumyd[i]
next
maxnu=max(lena,leyd)
i1=0
i2=1
priz=.t.
do while priz
   outl=space(44)+'|'
   priz=.f.
   if i1<=lena
      if i1=0
         outl='                Копейки прош.мес. '+STR(kop/100,9,2)+' |'
O7='Копейки пpош.мес    '+str(kop/100,9,2)
select d
use zatrat
append blank
replace nach with o7
use
         priz=.t.
      else
         do while i1<=lena
            if sumna[i1]<>0
               exit
            endif
            i1=i1+1
         enddo
         if i1<=lena
            wrk=kodna[i1]
            sele svo
            seek wrk
            outl=' '+STR(timeconv(dnei[i1],1)/100,11,2)+'  '+wrk+' '+naim+;
                 ' '+STR(sumna[i1]/100,9,2)+' |'
o=wrk+' '+naim+' '+str(sumna[i1]/100,9,2)
            priz=.t.
         endif
      endif
      i1=i1+1
   endif
   if i2<=leyd
      do while i2<=leyd
         if sumyd[i2]<>0
            exit
         endif
         i2=i2+1
      enddo
      if i2<=leyd
         wrk=kodyd[i2]
         sele suder
         seek wrk
         outl=outl+' '+wrk+'  '+naim+' '+STR(sumyd[i2]/100,9,2)
o1=wrk+' '+naim+' '+str(sumyd[i2]/100,9,2)
sele d
use zatrat
append blank
replace nach with o
append blank
replace nach with o1
o1=' '
o=' '
use
         priz=.t.
      endif
      i2=i2+1
   endif
enddo
clear
o2=STR((SUMMAN+KOP)/100,9,2) && начислено
o3=str(summau/100,9,2)       && удежано
o4=str(ost/100,9,2)
O5=STR(A/100,9,2)
O6=STR(DOLG/100,9,2)
?o2
?o3
?o4
?o5
?o6
wait
sele d
use zatrat
append blank
replace nach with 'Итого               '+o2
replace nach with 'Итого               '+o3
A=KOP+SUMMAN-SUMMAU-OST + dolg
IF A<0
ELSE
replace nach with 'Остаток             '+O4
replace nach with 'К выдаче            '+O5
ENDIF
REPLACE NACH WITH 'Долг                '+O6
use
sele svo
use
sele suder
use
return
