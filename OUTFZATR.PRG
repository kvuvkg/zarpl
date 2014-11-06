para TypeZatr
SET COLOR TO &STDC
do PrepZatr
sele 0
USE VREMPERE
SET FILTER TO O_KSP=KODPOD
GO TOP
SELE BASEF
SET FILTER TO YS $ '01'
GO TOP
DO WHILE .NOT.EOF()
   do RecoZatr
   sele basef
   skip
ENDDO
do RecoVrem
SELE BASEF
SET FILTER TO
SELE skr
USE
SELE svo
USE
SELE zatrat
USE
do ZatrOutf with TypeZatr
RETURN
proc PrepZatr
if select('svo')=0
   SELE 0
   USE SVO index svoindex
endif
SELE 0
USE SPSZ
SELE 0
USE ZATRAT
ZAP
SELE SPSZ
DO WHILE .NOT. EOF()
 SELE ZATRAT
 APPE BLANK
 REPL KZ WITH SPSZ->KPS,NAIM WITH SPSZ->NAIM,KPR WITH PRED_KP
 SELE SPSZ
 SKIP
ENDDO
SELE spsz
USE SKR
return
*
proc RecoZatr
priv vnnu, nnu, wrk
for vnnu=1 to 12
  nnu=ltrim(str(vnnu,2))
  wrk=basef->vo&nnu
  if val(wrk)<>0
     SELE SVO
     *LOCA FOR BASEF->VO&NNU=KVOY
     if wrk<>KVOY .or. eof()
        seek wrk   &&BASEF->VO&NNU
     endif
     IF VAL(KPS)=0
      SELE SKR
      if BASEF->KR<>KKP .or. eof()
         LOCA FOR BASEF->KR=KKP
      endif
      KZT=SKR->KPS
     ELSE
      KZT=SVO->KPS
     ENDIF
     SELE ZATRAT
     if KZT<>KZ .or. eof()
        LOCA FOR KZT=KZ
     endif
     REPL SUMMA WITH BASEF->SVON&NNU+SUMMA
     IF .NOT. (VAL(MESPOD)-VAL(BASEF->KMN&NNU)<9 .AND. VAL(MESPOD)-VAL(BASEF->KMN&NNU)>=0 ;
      .OR. VAL(MESPOD)-VAL(BASEF->KMN&NNU)<-3)
      REPL SUMMAB WITH BASEF->SVON&NNU+SUMMAB
     ENDIF
  endif
  SELE BASEF
next
return
*
proc RecoVrem
SELE VREMPERE
DO WHILE .NOT.EOF()
 SELE SVO
 if VREMPERE->O_VO<>KVOY
    seek VREMPERE->O_VO
 endif
 IF VAL(KPS)=0
  SELE BASEF
  GO VREMPERE->O_BASE_R
  SELE SKR
  if BASEF->KR<>KKP
     LOCA FOR BASEF->KR=KKP
  endif
  KZT=SKR->KPS
 ELSE
  KZT=SVO->KPS
 ENDIF
 SELE ZATRAT
 if KZT<>KZ
    LOCA FOR KZT=KZ
 endif
 REPL SUMMA WITH SUMMA-VREMPERE->O_SVON
 IF .NOT. (VAL(MESPOD)-VAL(VREMPERE->O_KM)<9 .AND. VAL(MESPOD)-VAL(VREMPERE->O_KM)>=0 ;
  .OR. VAL(MESPOD)-VAL(VREMPERE->O_KM)<-3)
  REPL SUMMAB WITH SUMMAB-VREMPERE->O_SVON
 ENDIF
 SELE VREMPERE
 SKIP
ENDDO
return
*
proc ZatrOutf
para typez
if pcount()<1
   typez='*'
endif
sele 0
use zatrat
ITOG=0
ITOGB=0
do ShapZatr
set filter to summa<>0
go top
DO WHILE .NOT. EOF()
  if prow()>Page_Size .or. row()>Page_Size
    do EndPage with 'ShapZatr'
  endif
  ? '      '+KZ+'       '+NAIM+'                 '+STR(SUMMA/100,13,2)
 ITOG=ITOG+SUMMA
 SKIP
ENDDO
if prow()>=Page_Size-4 .or. row()>=Page_Size-4
  do EndPage with 'ShapZatr'
endif
? ''
? '‚α¥£®                                          '+STR(ITOG/100,18,2)
? ''
? 'Ά β.η. Ά αη¥β ΅γ¤γι¨ε ―¥ΰ¨®¤®Ά  '
set filter to summab<>0
GO TOP
DO WHILE .NOT. EOF()
  if prow()>Page_Size .or. row()>Page_Size
    do EndPage with 'ShapZatr'
  endif
  ? '      '+KZ+'       '+NAIM+'                 '+STR(SUMMAB/100,13,2)
 ITOGB=ITOGB+SUMMAB
 SKIP
ENDDO
if prow()>=Page_Size-3 .or. row()>=Page_Size-3
  do EndPage with 'ShapZatr'
endif
? ''
? '‚α¥£®                                          '+STR(ITOGB/100,18,2)
do DaDoPo with 'ShapZatr'
sele zatrat
use
return
*
proc ShapZatr
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                ‚…„‘’ ’H…‘…H ‡€€’H‰ ‹€’›'
   ? '                   ‚„€ ‡‚„‘’‚…HH›• ‡€’€’  '
   ? '                ‡€ '+if(typez<>'*',mesname,Tek_M_N)+' '+Tek_God
endif
? substr(if(typez<>'*',kodpod+' '+NAMEPOD,'')+space(58),1,58)+'‹¨αβ '+;
  substr(str(100+Page_Nom,3),2,2)+'-'+if(typez<>'*',mespod,Tek_Mes_P)
type zatr.sha
return
*
