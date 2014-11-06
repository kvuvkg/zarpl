PARAMETER PAR
priv prm1,prm2
if .not.(par $ 'PS')
   return
endif
declare allna[12]
setcolor(stdc)
IF PAR='P'
 PRM1='ПОВРЕМЕHHОЙ'
 PRM2='ПОВРЕМЕHЩИKАМ'
ELSE
 PRM1='СДЕЛЬHОЙ'
 PRM2='СДЕЛЬЩИKАМ'
ENDIF
SUMD=0
RZKTU=0
zosd=0
SELE 0
USE TSZ
zap
if type('tsz->ksp')="C" .and. type('ktu_LKB')="N" .and. ktu_BRIG
   ksp_ssil='tsz->ksp'
   ktu_BRIG=.t.
else
   ksp_ssil='M->kodpod'
   ktu_BRIG=.f.
endif
SELE 0
USE SVO index svoindex
SELE 0
USE SSP
IF ktu_BRIG
 ktu_N=ktu_LKB
ELSE
 ktu_N=len(ksp)
ENDIF
left_kod=left(kodpod,ktu_N)
set filter to z='+' .and. left(ksp,ktu_N)=left_kod
go top
DO WHILE .NOT. EOF()
   sele basef
   use
   sele ssp
   kodpod=ksp
   namepod=nbrig
   mespod=mes
   dm=d
   ok=.f.
   do MountDsk with dm,kodpod,ok,.t.
   if .not. ok
      skip
      loop
   endif
   do OpenABC with "A",'000'
   sele super
   SET FILTER TO RPV $ '012'
   seek kodpod
   if eof()
      set filter to
      sele svo
      use
      sele tsz
      use
      sele ssp
      use
      return
   endif
   IF .NOT. ktu_BRIG
    Page_Nom=1
    do ShapVeKT
   ENDIF
   SUMI=0
   ZOSI=0
   VSRZ=0
   sele super
   DO WHILE .NOT.EOF() .and. ksp=kodpod
    sele basef
    go super->base_rec
    if ys<>'0'
       sele super
       skip
       loop
    endif
    IF .NOT. ktu_BRIG
     i=1+int(len(trim(vo1)+trim(vo2)+trim(vo3)+trim(vo4)+trim(vo5)+trim(vo6)+;
         trim(vo7)+trim(vo8)+trim(vo9)+trim(vo10)+trim(vo11)+trim(vo12))/len(vo1))
     if prow()+i>Page_Size .or. row()+i>Page_Size
       do EndPage with 'ShapVeKT'
     endif
     ? TN+' '+super->fio+' '+ STR(OKS/100,9,2)
    ENDIF
    STOR 0 TO SUM
    ZOS=0
    NU='1'
    for vnu=1 to 12
     nu=ltrim(str(vnu))
     wrkvo=vo&nu
     if wrkvo<>'   '
        SELE SVO
        seek basef->VO&NU
        sele basef
        IF wrkvo<>'399' .and. ('1' $ left(svo->zmo,2))
         IF substr(svo->zmo,3,1)="1"
          ZOS=ZOS+basef->SVON&NU
         ENDIF
         sele basef
         IF .NOT. ktu_BRIG
          if prow()>Page_Size .or. row()>Page_Size
            do EndPage with 'ShapVeKT'
          endif
          ? '  '+VO&NU+'    '+SVO->NAIM+'   '+STR(SVON&NU/100,10,2)
         ENDIF
         STOR SUM+SVON&NU TO SUM
        ENDIF
     endif
    next
    sele basef
    RZ=ZOS*KTY/100
    IF .NOT. ktu_BRIG
     if prow()>Page_Size .or. row()>Page_Size
       do EndPage with 'ShapVeKT'
     endif
     ? 'Итого по таб. номеру:       '+STR(SUM/100,9,2)+' '+STR(ZOS/100,10,2)+;
     ' '+STR(KTY/100,7,2)+' '+STR(RZ/100,9,2)
    ENDIF
    SUMI=SUMI+SUM
    ZOSI=ZOSI+ZOS
    VSRZ=VSRZ+RZ
    SELE TSZ
    APPE BLANK
    REPL TABN with basef->TN,SUMN WITH SUM,ZRPKTU WITH RZ,;
         base_r with super->base_rec
    if ktu_BRIG
       REPL ksp with kodpod
    endif
    sele super
    SKIP
   ENDDO
   sele basef
   IF .NOT. ktu_BRIG
    if prow()>Page_Size .or. row()>Page_Size
      do EndPage with 'ShapVeKT'
    endif
    ? 'Всего по подразделению      '+STR(SUMI/100,9,2)+' '+STR(ZOSI/100,10,2)+'         '+;
    STR(VSRZ/100,9,2)
   ENDIF
    sumd=sumd+sumi
    RZKTU=RZKTU+VSRZ
    zosd =zosd +zosi        &&  ?????
   sele super
   SET FILTER TO
   IF .NOT. ktu_BRIG
    do EndPage
   ENDIF
   sele ssp
   skip
ENDDO
SET CONS ON
if rzktu=0   && VSrz=0
   do ErrorTxt with ' Hе начислено '+prm1+' з/п или все KТУ=0 !!! '
   set print off
   tp_out=0
   sele ssp
   use
   sele svo
   use
   sele tsz
   use
   return
endif
SPRI=0
PFZP=0
SFZP=0
KFZP=0
PFMP=0
SFMP=0
KFMP=0
set color to
CLEAR
setcolor(ecol)
@ 24,0
@ 24,0 say 'Введите фонд зарплаты для расчета приработка или ┘/{ВВОД} для ввода приработка'
setcolor(stdc)
fond_zp=0.00
sumd=sumd/100.00
@ 0,0 say ' Hачисленная заработная плата '+str(sumd)+;
          '. Фонд з/п ' get fond_zp picture '####.##'
read
clear
if lastkey()=27
   do EndKTU
   return
endif
if fond_zp>0
   spri=fond_zp - sumd
endif
? ''
? '       ВЕДОМОСТЬ HАЧИСЛЕHИЯ ПРЕМИЙ '+PRM2+' С УЧЕТОМ KТУ ЗА '+mesname
type ktu1.sha
set curs on
keyboard ''
@ 8,9 GET SPRI PICT "####.##"
READ
if lastkey()=27
   do EndKTU
   return
endif
@ 8,9 SAY str(SPRI,7,2)
KPRI=SPRI*100/RZKTU
IF PAR='S'
 IF SPRI<>0
  KPRI=SPRI*100/RZKTU
 ELSE
  keyboard ''
  @ 8,19 GET KPRI PICT "##.###"
  READ
  if lastkey()=27
     do EndKTU
     return
  endif
  SPRI=KPRI*RZKTU/100
 ENDIF
ENDIF
@ 8,9 SAY STR(ROUND(SPRI,2),7,2)
@ 8,19 SAY STR(ROUND(KPRI,3),6,3)
keyboard ''
@ 8,27 GET PFZP PICT "###.###"
READ
if lastkey()=27
   do EndKTU
   return
endif
@ 8,27 SAY str(pfzp,7,3)
IF PFZP<>0
 SFZP=ZOSd*PFZP/10000 &&&&&&&&&&& ZOSI*PFZP/10000
 KFZP=SFZP*100/RZKTU
ELSE
 keyboard ''
 @ 8,35 GET SFZP PICT "####.##"
 READ
 if lastkey()=27
    do EndKTU
    return
 endif
 @ 8,35 SAY str(SFZP,7,2)
 IF SFZP<>0
  PFZP=SFZP*10000/zosd       &&&& ZOSI
  KFZP=SFZP*100/RZKTU
 ELSE
  keyboard ''
  @ 8,45 GET KFZP PICT "##.###"
  READ
  if lastkey()=27
     do EndKTU
     return
  endif
  SFZP=KFZP*RZKTU/100
  PFZP=SFZP*10000/zosd       &&&&& ZOSI
 ENDIF
ENDIF
@ 8,27 SAY STR(ROUND(PFZP,1),4,1)
@ 8,35 SAY STR(ROUND(SFZP,2),7,2)
@ 8,45 SAY STR(ROUND(KFZP,5),7,5)
keyboard ''
@ 8,53 GET PFMP PICT "###.###"
READ
if lastkey()=27
   do EndKTU
   return
endif
@ 8,53 SAY str(PFMP,7,3)
IF PFMP<>0
 SFMP=ZOSI*PFMP/10000
 KFMP=SFMP*100/RZKTU
ELSE
 keyboard ''
 @ 8,61 GET SFMP PICT "####.##"
 READ
 if lastkey()=27
    do EndKTU
    return
 endif
 @ 8,61 SAY str(SFMP,7,2)
 IF SFMP<>0
  PFMP=SFMP*10000/zosd              &&&&& ZOSI
  KFMP=SFMP*100/RZKTU
 ELSE
  keyboard ''
  @ 8,71 GET KFMP PICT "##.###"
  READ
  if lastkey()=27
     do EndKTU
     return
  endif
  SFMP=KFMP*RZKTU/100
  PFMP=SFMP*10000/zosd              &&&&& ZOSI
 ENDIF
ENDIF
set curs off
@ 8,53 SAY STR(ROUND(PFMP,1),7,3)
@ 8,61 SAY STR(ROUND(SFMP,2),7,2)
@ 8,71 SAY STR(ROUND(KFMP,5),7,5)
   @ 8,0
   @ 7,78 say ''
? STR(ROUND(SPRI,2),16,2)+STR(ROUND(KPRI,3),9,3)+STR(ROUND(PFZP,3),9,3)+;
  STR(ROUND(SFZP,2),9,2)+STR(ROUND(KFZP,5),9,5)+STR(ROUND(PFMP,3),9,3)+;
  STR(ROUND(SFMP,2),9,2)+STR(ROUND(KFMP,5),9,5)
sele tsz
if ktu_BRIG
   index on ksp to tsz
endif
set rela to &ksp_ssil + tabn into super
go top
if tp_out=1
   SET CONS OFF
else
   clear
endif
if ! eof()
 do ShapKTU
 stor 0 TO SNI_v,ZI_v,SN1I_v,SN2I_v,SN3I_v,VSEGOI_v
 Page_Nom=1
 DO WHILE .NOT. EOF()
  sele basef
  use
  sele ssp
  loca for ksp=&ksp_ssil
  kodpod=ksp
  namepod=nbrig
  mespod=mes
  dm=d
  ok=.f.
  do MountDsk with dm,kodpod,ok,.t.
  if .not. ok
     sele tsz
     do while ! eof() .and. &ksp_ssil = kodpod
        skip
     enddo
     loop
  endif
  do OpenABC with "A",'000'
  if prow()>=Page_Size-3 .or. row()>=Page_Size-3
    do EndPage with 'ShapKTU'
  endif
  ? ''
  ? ' '+kodpod+' '+namepod
  ? ''
  SELE TSZ
  stor 0 TO SNI,ZI,SN1I,SN2I,SN3I,VSEGOI
  DO WHILE &ksp_ssil = kodpod .and. .NOT.EOF()
   sele basef
   go tsz->base_r
   stor 0 to sn2,sn3
    SN1=TSZ->ZRPKTU*KPRI
   i=AddNewK('399','399',sn1)
   if sfzp<>0
      SN2=TSZ->ZRPKTU*KFZP
      wrk=if(par='P','401','402')
      do AtAndRe with wrk
      i=AddNewK(wrk,wrk,sn2)
   endif
   if sfmp<>0
      SN3=TSZ->ZRPKTU*KFMP
      wrk1=if(par='P','501','502')
      do AtAndRe with wrk1
      i=AddNewK(wrk1,wrk1,sn3)
   endif
   VSEGO=TSZ->SUMN+SN1+SN2+SN3
   if prow()>Page_Size .or. row()>Page_Size
     do EndPage with 'ShapKTU'
   endif
   ? TSZ->TABN+' '+super->FIO+STR(TSZ->SUMN/100,10,2)+STR(TSZ->ZRPKTU/100,8,2)+;
    STR(SN1/100,8,2)+STR(SN2/100,8,2)+STR(SN3/100,8,2)+STR(VSEGO/100,9,2)
   SN1I=SN1I+SN1
   SN2I=SN2I+SN2
   SN3I=SN3I+SN3
   VSEGOI=VSEGOI+VSEGO
   SELE TSZ
   sni=sni+SUMN
   zi =zi +ZRPKTU
   SKIP
  ENDDO
  if prow()>=Page_Size .or. row()>=Page_Size
    do EndPage with 'ShapKTU'
  endif
  ? ''
  ? ' Всего по подразделению    '+STR(SNI/100,10,2)+STR(ZI/100,8,2)+;
  STR(SN1I/100,8,2)+STR(SN2I/100,8,2)+STR(SN3I/100,8,2)+STR(VSEGOI/100,9,2)
  SELE TSZ
  SNI_v   = SNI_v    + SNI
  ZI_v    = ZI_v     + ZI
  SN1I_v  = SN1I_v   + SN1I
  SN2I_v  = SN2I_v   + SN2I
  SN3I_v  = SN3I_v   + SN3I
  VSEGOI_v= VSEGOI_v + VSEGOI
 ENDDO
 if ktu_BRIG
  if prow()>=Page_Size .or. row()>=Page_Size
    do EndPage with 'ShapKTU'
  endif
  ? ''
  ? ' Всего ................... '+STR(SNI_v/100,10,2)+STR(ZI_v/100,8,2)+;
    STR(SN1I_v/100,8,2)+STR(SN2I_v/100,8,2)+STR(SN3I_v/100,8,2)+;
    STR(VSEGOI_v/100,9,2)
 endif
 do DaDoPo with 'ShapKTU'
endif
do EndKTU
RETURN
*
proc EndKTU
sele ssp
use
sele tsz
use tsz
zap
use
sele svo
use
if file('tsz.ntx')
   erase tsz.ntx
endif
return
*
proc ShapVeKT
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '        ВЕДОМОСТЬ HАЧИСЛЕHИЯ '+PRM1+' ЗАРАБОТHОЙ ПЛАТЫ '
   ? '               С УЧЕТОМ KТУ ЗА '+mesname+' '+Tek_God
endif
? substr(kodpod+' '+NAMEPOD+space(60),1,60)+'Лист '+substr(str(100+Page_Nom,3),2,2)+'-'+mespod
type vekt.sha
return
*
proc ShapKTU
@ 0,0 say ''
? space(68)+'Лист '+substr(str(100+Page_Nom,3),2,2)+'-'+mespod
type ktu.sha
return
*
proc AtAndRe
para kod
priv i,sel_c
sel_c=str(select())
sele basef
i=at('*'+kod+mespod,'*'+vo1+kmn1+'*'+vo2+kmn2+'*'+vo3+kmn3+'*'+vo4+kmn4+;
                   '*'+vo5+kmn5+'*'+vo6+kmn6+'*'+vo7+kmn7+'*'+vo8+kmn8+;
                   '*'+vo9+kmn9+'*'+vo10+kmn10+'*'+vo11+kmn11+'*'+vo12+kmn12)
if i<>0
   i=ltrim(str(int(i/(len(vo1)+len(kmn1)+1))+1,2))
   repl tsz->sumn with tsz->sumn - basef->svon&i
endif
sele &sel_c
return
