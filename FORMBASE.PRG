parameter next_func
setcolor(stdc)
*НАКОПЛЕН SEBA 421
close data
do while .t.
 set color to /
 clear
 setcolor(ucol)
 @ 0,0 say ' Технологичеcкие Операции '
 @ 1,0 say ' '+Tek_M_N+' '+Tek_God+' '
 close data
 sele 0
 use ssp
 setcolor(stdc)
 @ 3,0 say ' Вcего '+ltrim(str(recc(),4))+' подразд. '
 imax=0
 setcolor(mcol)
 set key -1 to PressCr
 set key -2 to PressCr
 set key -3 to PressCr
 set key -4 to PressCr
 set key -5 to PressCr
* set key -22 to PressCr   && ctrl-F3
 set key -27 to PressCr   && ctrl-F8
 set key 22 to PressCr
 set key  7 to PressCr
 set key 083 to PressCr
 set key 115 to PressCr
 set key 073 to PressCr
 set key 105 to PressCr
 keyboard ''
 PressKey=0
 xb=0
 yb=0
 do ListMenu with '',1,27,20,1,SprNom,SprTop,;
'str(recno(),4)+". "+ksp+" "+nbrig+" "+d+if(z="+"," *закреплен* ",space(13))+mes+" "',;
'{Esc}/{Ключ}-конец {Ins}/{ВСТ}-созд. {Del}/{УДЛ}-удал. F2-коррект. F3-закреплен.'+;
'F4-перейти к новому меcяцу   F5-указать код нового меcяца  F6-"сжатие" данных   '+;
' "s" - корректировка справочников  "i" - обработка информации  ',;
    'Список структурных подразделений','',xb,yb
 set curs off
 setcolor(stdc)
 @ 23,0 clear
 set key -1 to
 set key -2 to
 set key -3 to
 set key -4 to
 set key -5 to
 set key -8 to
 set key -22 to
 set key -27 to
 set key 22 to
 set key  7 to
 set key 083 to
 set key 115 to
 set key 073 to
 set key 105 to
 if SprNom=0
    exit
 endif
 if PressKey=83 .or. PressKey=115
    next_func=2
    exit
 endif
 if PressKey=73 .or. PressKey=105
    next_func=1
    exit
 endif
 sele ssp
 go   SprTop
 skip SprNom - 1
 if dele() .and. PressKey<>22 .and. PressKey<>-3 .and. PressKey<>-27
    do ErrorTxt with 'Вы ошиблись. Запись удалена.'
    loop
 endif
 if PressKey=0 .and. lastkey()=13
    PressKey=13
 endif
 if PressKey<>0
   MyPress=PressKey
   setcolor(stdc)
   clear
   setcolor(ucol)
   @ 0,0 say ' Технологичеcкие Операции '
   @ 1,0 say ' '+Tek_M_N+' '+Tek_God+' '
   setcolor(stdc)
   do case
     case MyPress=-1 .or. MyPress=13
        do ModiPodr
     case MyPress=-2
        do MoveFile
     case MyPress=-3
        do GoToNewM
     case MyPress=-4
        do SetMesP
     case MyPress=-5
        do ComprPod
     case MyPress=-27  && реорганизация поcле изменения cтруктуры BASE и SEBA
        do Reorga
     case MyPress=22
        new_rn=0
        do NewPodr with new_rn
     case MyPress=7
        do DelePodr
   endcase
   loop
 endif
enddo
close data
return
*
proc DelePodr
r_del=' '
do AskYesNo with r_del,24,;
            'Удалить подразделение '+ksp+'. '+trim(nbrig)+;
            ' ? ',ecol
if r_del<>'D'
   return   && loop
endif
if z='+'
   ok=.f.
   dm=d
   kspm=trim(ksp)
   do MountDsk with dm,kspm,ok,.t.
   if .not.ok
      return && loop
   endif
   erase &dm.:base&kspm..dbf
   erase &dm.:seba&kspm..dbf
   repl z with '-'
endif
sele 0
use superlist alias super
dele all for ksp=ssp->ksp .or. tn=replicate('*',len(tn))
pack
use
do IndexSup
use
sele ssp
delete
pack
return
*
proc ComprPod
sele ssp
kodpod=ksp
namepod=nbrig
mespod=mes
dm=d
ok=.f.
do MountDsk with dm,kodpod,ok,.t.
if .not. ok
   return && loop
endif
close data
do OpenABC with 'ABC'
setcolor(stdc)
clear
setcolor(ucol)
@ 1,1 say ' "Сжатие" информации '
sele basef
loca for tn=replicate('*',len(tn))
do while .not. eof()
   i=(recno()-1)*12+1
   delete
   sele sebaf
   go i
   dele next 12
   sele basef
   cont
enddo
sele basef
pack
sele sebaf
pack
sele super
use superlist index superpono alias super
do ReorList
close data
do IndexSup
return
*
proc NewPodr
para new_reco
new_reco=0
setcolor(bcol)
@ 2,35,13,76 box frame+'░'
dm=' '
zm='-'
tem=space(len(mes))
kspm=space(len(ksp))
nbrigm=space(len(nbrig))
set curs on
@  4,37 say 'Код           ';
       get kspm valid(UniqKodS('ksp',kspm,-999);
                      .and.len(trim(kspm))<>0)
@  5,37 say 'Наименование  ' get nbrigm
@  6,37 say 'Адрес диска   ';
       get dm valid (FldOkDZ('FReadSSP',2,'M->DM'))
@  7,37 say 'Текущий месяц ';
       get tem picture '99';
       valid (val(tem)>0 .and. val(tem)<13;
                         .and. .not.(' ' $ tem))
read
set curs off
if lastkey()<>27
   loca for dele()
   if eof()
      append blank
   else
      recall
   endif
   new_reco=recno()
   repl ksp with kspm, nbrig with nbrigm,;
        d with dm, z with '-', mes with tem
   skip -5
   if bof()
      go top
   endif
   SprTop=recno()
   SprNom=1
endif
return
*
proc ModiPodr
setcolor(bcol)
@ 3,35,11,76 box frame+'░'
@ 3,36 say trim(nbrig)
ok=.f.
dm=d
zm=z
tem=mes
do while .not. ok
   ok=.f.
   setcolor(stdc)
   new_ksp=ksp
   set curs on
   @  5,37 say 'Код           ' get new_ksp;
           valid(UniqKodS('ksp',new_ksp,recno());
                 .and.len(trim(new_ksp))<>0)
   @  6,37 say 'Наименование  ' get nbrig
   @  7,37 say 'Адрес диска   ' get dm;
           valid (FldOkDZ('FReadSSP',2,'M->DM'))
   @  8,37 say 'Закрепление    '+zm
   @  9,37 say 'Текущий месяц ' get tem picture '99';
                               valid (val(tem)>0 .and. val(tem)<13;
                                      .and. .not.(' ' $ tem))
   read
   set curs off
   if lastkey()=27
      ok=.f.
      exit
   endif
   if new_ksp<>ksp .and. z='+'
      setcolor(ucol)
      @ 22,0 clear to 23,79
      @ 22,1 say ' В случае сбоя необходимо: восстановить SSP.dbf SUPERLIS.dbf и диск '+ksp
      @ 23,1 say '                           запустить ZARP/i'
      rrr=' '
      do AskYesNo with rrr,24,;
                  'Переименовать подразделение '+ksp+'. '+;
                  trim(nbrig)+' ? ',ecol
      if rrr<>"D"
         loop
      endif
      sele ssp
      kodpod=ksp
      namepod=nbrig
      mespod=mes
      dm=d
      ok=.f.
      do MountDsk with dm,kodpod,ok,.t.
      if .not. ok
         loop
      endif
      setcolor(ecol)
      @ 24,0
      setcolor(stdc)
      do OpenABC with 'ABC','000'
      sele basef
      use
      sele sebaf
      use
      rename &dm.:base&kodpod..dbf to &dm.:base&new_ksp..dbf
      rename &dm.:seba&kodpod..dbf to &dm.:seba&new_ksp..dbf
      sele super
      use superlist alias super
         repl all ksp with new_ksp for ksp=kodpod
      use
      do IndexSup
      ok=.t.
   endif
enddo
sele ssp
if ok
   repl d with dm, z with zm, mes with tem, ksp with new_ksp
endif
return
*
Procedure MoveFile
if z='+'
   do ErrorTxt with 'Вы ошиблись. Закрепление уже было произведено !!!'
   return
endif
ok=.f.
dm=d
kspm=trim(ksp)
do MountDsk with dm,kspm,ok,.f.
if ok
   setcolor(ecol)
   @ 24,0
   @ 24,7 say ' Копируем файлы... Ждите... '
   sele 0
   use base
   copy stru to &dm.:base&kspm
   use seba
   copy stru to &dm.:seba&kspm
   sele ssp
   if lastkey()<>27
      repl z with '+'
   endif
   setcolor(stdc)
endif
return
*
proc GoToNewM
SELE 0
USE SVO index svoindex
i=recc()
declare kodna[i], maskna[i]
do ReadMskM with kodna, maskna
sele 0
use suder index suderind
end_shap=5
itog_l=20
kop_row=19
sele 0
use pasp
tek_mes=tm
use skn
sele ssp
set filter to .not.dele() .and. z='+'
go top
do while .not. eof()
   if mes<>tek_mes
      setcolor(stdc)
      @ 2,0 clear
      setcolor(bcol)
      @ 2,1,7,70 box frame+'░'
      keyboard ''
      setcolor(ecol)
      @ 4,3 say 'Переход к текущему месяцу подразделения '+;
                ksp+'. '+trim(nbrig)
      setcolor(ecol)
      @ 5,3 say 'Нажмите: ┘/{Ввод} - согласие, {Esc}/{Ключ} - отказ'
      i=0
      do while i<>13 .and. i<>27
         i=inkey(0)
      enddo
      setcolor(bcol)
      @ 5,3,5,69 box replicate('░',9)
      if i=13
         old_mes=mes
         do SetMes with old_mes
      endif
   endif
   sele ssp
   skip
enddo
sele skn
use
sele suder
use
sele svo
use
sele ssp
set filter to
rele kodna, maskna
return
*
Procedure SetMes
parameters old_mes
priv i,j,kodpod
qq=0
ok=.f.
dm=d
kspm=ksp
do MountDsk with dm,kspm,ok,.t.
if ok
   mespod=ssp->mes
   kodpod=kspm
   do OpenABC with 'AC','000'
   sele basef
   set filter to rpv $ '12' .and. ys $ '01'
   go top
   setcolor(stdc)
   reca13=.f.
   if val(tek_mes)=1
      rr=' '
      do AskYesNo with rr,24,;
         'Производить расчет 1/12 суммы премий и суммы для 13-й з/п ? ',ecol
      if rr='D'
         reca13=.t.
      endif
      set color to
      @ 24,0
      setcolor(stdc)
   endif
   do while .not. eof()
      sele basef
      @ 5,3 say '                                     '
      @ 5,5 say 'Табельный номер '+tn
      if reca13
         do Prem13ZP
      endif
      i=(recno()-1)*12+val(mespod)
      sele sebaf
    D100 = 0
      go i
      for i=1 to 12
         j=if(i<10,str(i,1),str(i,2))

         repl kmn&j with basef->kmn&j,vo&j with basef->vo&j,;
              svon&j with basef->svon&j,;
           dnch&j  with basef->dn&j /100 ,;
              uder   with basef->sku1+basef->sku2+basef->sku3+basef->sku4+;
                          basef->sku5+basef->sku6+basef->sku7+basef->sku8+;
                          basef->sku9+basef->sku10+basef->sku11+basef->sku12
              *kmu&j with basef->kmu&j,ku&j with basef->ku&j,sku&j with basef->sku&j,dn&j with basef->dn&j,
      next
      repl cd  with basef->cd,  oz  with basef->oz,;
           kty with basef->kty, kpm with basef->kpm
      sele basef
      skip
   enddo
   *отметка о выполненном переноcе
   *Вызов Raschety -- вычисление средних, больничных
   sele basef
   go top
   do Raschety
   *просмотр долговременной и уменьшение сумм удержаний, если они не 0,
   *на значение из оперативной информации
   *зануление информации и перенос постоянной и долговременной в оперативную
   sele basef
   set filter to rpv $ '012' .and. ys='0'
   oldpod=mespod
   mespod=tek_mes
   do Umolch
   sele sebaf
   use
   sele basef
   set filter to
   use aliment
   zap
   use bank
   zap
   use bank7
   zap
   use vrempere
   zap
   if file('zrpl.dbf')
      use zrpl
      zap
   endif
   use
   sele ssp
   repl mes with tek_mes
endif
sele f
use skl
do while.not.eof()
repl ch with qq,pr with 0,spr with 0,szp with 0,fond with 0,kty with 0
skip
enddo
use po_1
zap
use po_2
zap
use po_3
zap
use po_4
zap
use po_5
zap
use po_6
zap
use po_7
zap
return
*
proc Umolch
* MESPOD   C(2)  Расчетный месяц
* TEK_GOD  C(4)  Расчетный год
* DN_ARR[i] массив рабочих дней из 12-ти  элементов,каждый N(2)
* CHAS_ARR[i] массив рабочих часов из 12-ти элементов,каждый N(6,2)
PRIV KM,GOD,A,I,J,II,JJ,D1,D2,vro_dn,vro_ch,t_k
KM=MESPOD
ikm=val(mespod)
if ikm<1 .or. ikm>12
   return
endif
*ikm=val(oldpod)     &&&& if(ikm1=1,12,ikm1-1)
GOD=TEK_GOD
sele basef
GO TOP
DO WHIL .NOT.EOF()
 tk=TestKale() && if(type('basef->kalendar')<>'C','',trim(basef->kalendar))
 vro_dn=dn_arr&tk[ikm]
 dni100  = vro_dn*100
 chas100 = timeconv(chas_arr&tk[ikm]*100,1)
 vro_ch  = chas100/100
qq=vro_ch
public  qq
 * Чистка
 sele basef
 i=recno()
 @ 5,3 say '                                     '
 @ 5,5 say 'Запись в архив '+tn
 sele sebaf
 go (i-1)*12+val(oldpod)   && ikm
 sele basef
 REPL DR WITH sebaf->CD,kpm WITH sebaf->OZ,;
      rabd&mespod with if(pts='2',chas100,dni100)
 REPL KMN1 WITH ' ',VO1 WITH ' ',DN1 WITH 0,SVON1 WITH 0,;
         KMN2 WITH ' ',VO2 WITH ' ',DN2 WITH 0,SVON2 WITH 0,;
         KMN3 WITH ' ',VO3 WITH ' ',DN3 WITH 0,SVON3 WITH 0
 REPL KMN4 WITH ' ',VO4 WITH ' ',DN4 WITH 0,SVON4 WITH 0,;
         KMN5 WITH ' ',VO5 WITH ' ',DN5 WITH 0,SVON5 WITH 0,;
         KMN6 WITH ' ',VO6 WITH ' ',DN6 WITH 0,SVON6 WITH 0
 REPL KMN7 WITH ' ',VO7 WITH ' ',DN7 WITH 0,SVON7 WITH 0,;
         KMN8 WITH ' ',VO8 WITH ' ',DN8 WITH 0,SVON8 WITH 0,;
         KMN9 WITH ' ',VO9 WITH ' ',DN9 WITH 0,SVON9 WITH 0
 REPL KMN10 WITH ' ',VO10 WITH ' ',DN10 WITH 0,SVON10 WITH 0,;
         KMN11 WITH ' ',VO11 WITH ' ',DN11 WITH 0,SVON11 WITH 0,;
         KMN12 WITH ' ',VO12 WITH ' ',DN12 WITH 0,SVON12 WITH 0
 REPL KMU1 WITH ' ',KU1 WITH ' ',SKU1 WITH 0,;
         KMU2 WITH ' ',KU2 WITH ' ',SKU2 WITH 0,;
         KMU3 WITH ' ',KU3 WITH ' ',SKU3 WITH 0
 REPL KMU4 WITH ' ',KU4 WITH ' ',SKU4 WITH 0,;
         KMU5 WITH ' ',KU5 WITH ' ',SKU5 WITH 0,;
         KMU6 WITH ' ',KU6 WITH ' ',SKU6 WITH 0
 REPL KMU7 WITH ' ',KU7 WITH ' ',SKU7 WITH 0,;
         KMU8 WITH ' ',KU8 WITH ' ',SKU8 WITH 0,;
         KMU9 WITH ' ',KU9 WITH ' ',SKU9 WITH 0
 REPL KMU10 WITH ' ',KU10 WITH ' ',SKU10 WITH 0,;
         KMU11 WITH ' ',KU11 WITH ' ',SKU11 WITH 0,;
         KMU12 WITH ' ',KU12 WITH ' ',SKU12 WITH 0
 
 
 REPL CD WITH 0,OZ WITH 0              && 17.03.89,11:56
 IF (RPV $ '12').AND.YS='0' && $ '01'
* Hачисления по умолчанию
  DO CASE
   CASE PTS='1'
    a=1  &&A=vro_dn     &&DN_ARR[VAL(KM)]
    *stor vro_dn to vro,a1 &&vro=vro_dn
   CASE PTS='2'
    *vro=vro_ch
    a=vro_ch/100 &&vro/100    &&stor vro/100 to a,a1 &&& a=vro/100
    *a1=100
   case PTS='3'
    *vro=vro_dn
**********
*    a=vro_dn  &&100 &&stor 100 to a,a1 &&A=100
*********
     a=vro_ch/100
    *a1=1
   othe
    return
  ENDC
if pts='1'.or.pts='2'
  REPL KMN1 WITH KM,VO1 WITH '10'+PTS,SVON1 WITH TestSvon(oks*a);
       DN1 WITH if(pts='2',vro_ch,VRO_dn)*100
else
repl kmn1 with km,vo1 with '103',svon with testsvon(oks),dn1 with vro_ch*100
endif 
  II=0
  DO WHIL II<6
   II=II+1
   I=STR(II,1)
   if val(NVU&I)<>0
    JJ=II+1
    J=STR(JJ,1)
   DO CASE
    CASE VAL(NHO&I)=0
     D1='000000'
    CASE VAL(SUBS(NHO&I,1,2))>50
     D1='19'+NHO&I
    OTHE
     D1='20'+NHO&I
   ENDC
   DO CASE
    CASE VAL(NKNO&I)=0
     D2='999999'
    CASE VAL(SUBS(NKNO&I,1,2))>50
     D2='19'+NKNO&I
    OTHE
     D2='20'+NKNO&I
   ENDC
  IF D1<=GOD+KM.AND.D2>=GOD+KM
   val_nvu=val(nvu&i)
   DO CASE
    CASE val_nvu>=301 .and. val_nvu<=330
    Oklad = oks
    if mPen > 0
       Oklad = mPen
    Endif
if val_nvu=322
    s=Oklad*(qq/100)*nsu&i/10000
else
     S=Oklad*a*nsu&i/10000 &&(OKS*VRO*NSU&I)/(a*100)
endif
    CASE val_nvu>=331 .and. val_nvu<=340
     S=NSU&I*A
case val_nvu=395
s=oks*nsu&i/10000
* -------СЕВЕРНЫЕ------
    case val_nvu=366
     s=MIN ( DOPS_MAXS , PrSr(21) ) * nsu&i / 10000
* ---------------------
    OTHE
     S=NSU&I
   ENDC
   REPL KMN&J WITH KM,VO&J WITH NVU&I,;
        SVON&J WITH TestSvon(S),DN&J WITH if(pts='2',VRO_ch,vro_dn)*100 &&VRO*100
  ENDI
  endi
  ENDD
* -------РАЙОННЫЕ-------
  if dops_rk<>0
     do RaiKoeff
  endif
* ----------------------
* Удержания по умолчанию
  IF PA<>0
   REPL SKU1 WITH PA*100,KU1 WITH '915',KMU1 WITH KM
  ENDI
  IF DR<>0
   REPL SKU2 WITH DR,KU2 WITH '909',KMU2 WITH KM
  ENDI
  REPL KU3 WITH '910',KMU3 WITH KM
  SELE SKN
  LOCA for KN=basef->NP
  sele basef
  IF skn->PMN=0
   REPL KU4 WITH '911',KMU4 WITH KM
  ENDI
  SELE basef
  IF CHP='1'
   REPL KU5 WITH '912',KMU5 WITH KM
  ENDI
  IF val(KPSS)<>0
   REPL KMU6 WITH KM
   DO CASE
    CASE KPSS='1'
     REPL KU6 WITH '914'
    CASE KPSS='2'
     REPL KU6 WITH '913'
   ENDC
  ENDI
  II=0
  DO WHIL II<6
   II=II+1
   i=str(ii,1)
   if val(UVU&I)<>0
    JJ=II+6
    J=IIF(JJ<10,STR(JJ,1),STR(JJ,2))
   DO CASE
    CASE VAL(UHO&I)=0
     D1='000000'
    CASE VAL(SUBS(UHO&I,1,2))>50
     D1='19'+UHO&I
    OTHE
     D1='20'+UHO&I
   ENDC
   DO CASE
    CASE VAL(UKNO&I)=0
     D2='999999'
    CASE VAL(SUBS(UKNO&I,1,2))>50
     D2='19'+UKNO&I
    OTHE
     D2='20'+UKNO&I
   ENDC
   IF D1<=GOD+KM.AND.D2>=GOD+KM
      DO CASE
       CASE UVU&I $ '*919*917*939'
         REPL KU&J WITH uvu&i,KMU&J WITH KM
       case uvu&i='921'
         REPL KU&J  WITH uvu&i,KMU&J WITH KM,;
              sku&j with if(uso&i-usu&i>0,usu&i,max(uso&i,0))
       OTHE
         REPL KU&J WITH UVU&I,KMU&J WITH KM,SKU&J WITH USU&I
      ENDC
   ENDI
   endi
  ENDD
 ENDI
 *
 first_ras=.t.
 do ReCalc with .f.
 sele basef
 *
 SKIP
ENDD
RETU
*
proc Reorga
priv r_r, r, rec_b
setcolor(ucol)
@ 24,0
@ 24,3 say 'Реорганизация хранимых данных'
sele 0
use superlist alias super
if (type('super->rpv')<>'C' .or. type('super->base_rec')<>'N' )
   if ! file('supernew.dbf')
      do ErrorTxt with;
         ' Измените структуру файла SuperLis и снова вызовите реорганизацию '
      use
      return
   endif
   sele super
   copy to work
   use
   sele 0
   use supernew
   copy stru to superlist
   use superlist alias super
   appe from work
   erase work.dbf
endif
sele super
use superlist index superpono alias super
sele ssp
set filter to .not.dele() .and. z='+'
go top
do while .not. eof()
  setcolor(stdc)
  @ 2,0 clear
  setcolor(ecol)
  @ 20,3 say ' Реорганизация подразделения '+;
            ksp+'. '+trim(nbrig)+'...'
  r_r=' '
  do AskYesNo with r_r,24,'...осуществлять ?    ',ecol
  @ 24,0
  if r_r='D'
     ok=.f.
     dm=d
     kspm=ksp
     kodpod=ksp
     do MountDsk with dm,kspm,ok,.t.
     if ok
        sele 0
        use &dm.:base&kspm alias basef
        if FreeCopy(int((header()+recsize()*recc())/4096)*4096,;
                    'Основного файла')
           r=' '
           do AskYesNo with r,24,;
           '...осуществлять реорганизацию основного файла ? ',ecol
           @ 24,0
           if r='D'
              setcolor(ecol)
              @ 24,0
              @ 24,3 say '...Ждите...Идет реорганизация основного файла...'
              copy to work&kspm
              use base
              copy stru to &dm.:base&kspm
              use &dm.:base&kspm alias basef
              append from work&kspm
              erase work&kspm..dbf
           endif
        endif
        do ReorList
        sele basef
        use
        if file(dm-':seba'-kspm-'.dbf')
           use &dm.:seba&kspm alias sebaf
           if FreeCopy(int((header()+recsize()*recc())/4096)*4096,;
                       'Архивного файла')
              r=' '
              do AskYesNo with r,24,;
              '...осуществлять реорганизацию архивного файла ? ',;
                          ecol
              @ 24,0
              if r='D'
                 setcolor(ecol)
                 @ 24,0
                 @ 24,3 say;
                 '...Ждите...Идет реорганизация архивного файла...'
                 copy to work&kspm
                 use seba
                 copy stru to &dm.:seba&kspm
                 use &dm.:seba&kspm alias sebaf
                 append from work&kspm
                 use
                 erase work&kspm..dbf
              endif
           endif
        endif
     endif
  endif
  setcolor(stdc)
  sele ssp
  skip
enddo
sele super
use
do IndexSup
if select('super')<>0
   sele super
   use
endif
sele ssp
set filter to
return
*
proc ReorList
setcolor(ecol)
@ 24,0
@ 24,3 say '...Ждите...Идет реорганизация списка сотрудников...'
sele basef
go top
do while ! eof()
   rec_b=recno()
   sele super
   seek kodpod+basef->tn
   if eof()
      sele basef
      repl tn with replicate('*',len(tn)), rpv with ' '
   else
      repl rpv with basef->rpv, base_rec with rec_b
   endif
   sele basef
   skip
enddo
return
*

proc Prem13ZP
priv n,nn,itsvo,itsvo13,end_rec
stor 0 to ITSVO,itsvo13
sele basef
N=RECNO()
SELE SEBAF
NN=(N-1)*12+1
GO NN
end_rec=nn+12  &&(N-1)*12+13
do WHILE recno()<>end_rec .and. ! eof()
 for vnnu=1 to 12
   nnu=ltrim(str(vnnu,2))
   wrk=vo&nnu
   if wrk<>' '
      j=ascan(kodna,wrk)
      if j>0 .and. j<=len(kodna)
         m1=maskna[j]
         oplats=svon&nnu
         if substr(m1,12,1)='1'
            itsvo13=itsvo13+oplats
         endif
         if substr(m1,22,1)='1'
            itsvo=itsvo+oplats
         endif
      endif
   endif
  next
  skip
enddo
sele basef
repl stz with round(zarp13/12.00,0), spvz with itsvo13,;
     dsp with round(itsvo/12.00,0)
return
*
