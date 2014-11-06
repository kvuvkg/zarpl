proc OutfKred
SELE KREDIT
SET FILTER TO VU='921'
GO TOP
if eof()
   set filter to
   return
endif
set rela to tn into super
DO WHILE .t.
 ko_kred=kredit->ko
 SELE SORG
 LOCA FOR KOO=KO_kred
 SELE KREDIT
 IT1=0
 IT2=0
 Page_Nom=1
 do ShapKred
 DO WHILE KO=ko_kred .AND. .NOT. EOF()
  if prow()>=Page_Size .or. row()>=Page_Size
    do EndPage with 'ShapKred'
  endif
  ? ' '+TN+' '+super->FIO+'  '+ND+'  '+SUBS(HO,3,2)+'.'+SUBS(HO,1,2)+;
  STR(SO/100,9,2)+STR(SKU/100,8,2)+STR((SO-SKU)/100,9,2)+' '+;
  SUBS(KPU,3,2)+'.'+SUBS(KPU,1,2)
  IT1=IT1+SO
  IT2=IT2+SKU
  SKIP
 ENDDO
 if prow()>=Page_Size .or. row()>=Page_Size
   do EndPage with 'ShapKred'
 endif
 ? ''
 ? ' ‚α¥£® ―® ―ΰ¥¤―ΰ¨οβ¨ξ                        '+STR(IT1/100,9,2)+;
 STR(IT2/100,8,2)+STR((IT1-IT2)/100,9,2)
 do Zaversh with 'ShapKred',it2
 IF .NOT. EOF()
  SKIP -1
 ELSE
  GO BOTTOM
 ENDIF
 if count_prn=1
    SELE BANK
    APPE BLANK
    REPL BANK->VU WITH KREDIT->VU,BANK->KO WITH KREDIT->KO,;
         BANK->SU WITH IT2
 endif
 SELE KREDIT
 SKIP
 if eof()
    exit
 endif
 do EndPage
ENDDO
sele kredit
set rela to
SET FILTER TO
SET PRINT OFF
RETURN
proc ShapKred
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                               ‚ … „    ‘ ’    '
   ? '              ……—‘‹…H ‘“ ‡€ ’‚€›, …’…HH›… ‚ K…„’ '
   do EndShap with 4
endif
? space(68)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type Kred.sha
return
*
proc OutfBIR
SELE KREDIT
SET FILTER TO VU='922'
GO TOP
if eof()
   set filter to
   return
endif
set rela to tn into super
DO WHILE .t.   && .NOT. EOF()
 ko_kred=kredit->ko
 SELE SORG
 LOCA FOR KOO=KO_kred
 SELE KREDIT
 IT=0
 Page_Nom=1
 do ShapBIR
 DO WHILE KO=ko_kred .AND. .NOT. EOF()
  if prow()>Page_Size .or. row()>Page_Size
    do EndPage with 'ShapBIR'
  endif
  ? '  '+NC+'    '+TN+' '+super->FIO+' '+STR(SVO/100,9,2)+' '+ND+STR(SO/100,10,2);
        +STR(SKU/100,10,2)
  IT=IT+SKU
  SKIP
 ENDDO
 if prow()>=Page_Size .or. row()>=Page_Size
   do EndPage with 'ShapBIR'
 endif
 ? ''
 ? ' ‚α¥£® ―® ―ΰ¥¤―ΰ¨οβ¨ξ'+space(41)+STR(IT/100,14,2)
 do Zaversh with 'ShapBIR',it
 IF .NOT. EOF()
  SKIP -1
 ELSE
  GO BOTTOM
 ENDIF
 if count_prn=1
    SELE BANK
    APPE BLANK
    REPL BANK->VU WITH KREDIT->VU,BANK->KO WITH KREDIT->KO,;
         BANK->SU WITH IT
 endif
 SELE KREDIT
 SKIP
 if eof()
    exit
 endif
 do EndPage
ENDDO
sele kredit
set rela to
SET FILTER TO
SET PRINT OFF
RETURN
proc ShapBIR
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                             ‚ … „    ‘ ’    '
   ? '                “„…†€H‰  ‘‹H’…‹H› „K“…H’€ (‚ ) '
   do EndShap with 4
endif
? space(67)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type Bir.sha
return
*
proc OutfVEDO
if select('SVO')=0
  sele 0
  use svo index svoindex
endif
if select('SUDER')=0
  sele 0
  use suder index suderind
endif
do ORscVedo with kolrabsv,kopsv,ostsv,dolgsv,kodna,sumnasv,dneisv,lena,;
                kodyd,sumydsv,leyd,;
  '           ‘Ά®¤­ ο Ά¥¤®¬®αβμ ­ η¨α«¥­¨© ¨ γ¤¥ΰ¦ ­¨© ―® §/― ','',''
if select('SVO')<>0
  sele svo
  use
endif
if select('SUDER')<>0
  sele suder
  use
endif
return
*
proc OutfSTUD
SELE KREDIT
SET FILTER TO VU='939'
GO TOP
if eof()
   set filter to
   return
endif
set rela to tn into super
Page_Nom=1
do ShapSTUD
DO WHILE .t.
 ko_kred=kredit->ko
 SELE SORG
 LOCA FOR KOO=ko_kred
 SELE KREDIT
 IT2=0
 DO WHILE KO=ko_kred .AND. .NOT. EOF()
    if prow()>Page_Size .or. row()>Page_Size
       do EndPage with 'ShapSTUD'
    endif
    ? '  '+NC+'   '+TN+'  '+super->fio+' '+STR(DN/100,8)+STR(SVO/100,12,2)+;
    STR(SKU/100,12,2)
    IT2=IT2+SKU
    SKIP
 ENDDO
 if prow()>=Page_Size .or. row()>=Page_Size
    do EndPage with 'ShapSTUD'
 endif
 ? ''
 ? ' ‚α¥£® ―® ―ΰ¥¤―ΰ¨οβ¨ξ                                 '+;
 STR(IT2/100,15,2)
 Do Zaversh with 'ShapSTUD',it2
 IF .NOT. EOF()
  SKIP -1
 ELSE
  GO BOTTOM
 ENDIF
 if count_prn=1
    SELE BANK
    APPE BLANK
    REPL BANK->VU WITH KREDIT->VU,BANK->KO WITH KREDIT->KO,;
         BANK->SU WITH IT2
 endif
 SELE KREDIT
 SKIP
 if eof()
    exit
 endif
 do EndPage
ENDDO
SET FILTER TO
set rela to
set print off
set cons on
RETURN
proc ShapSTUD
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                              ‚ … „    ‘ ’    '
   ? '                   ……—‘‹…H ‡€€’H‰ ‹€’› “—€™•‘ '
   do EndShap with 2
endif
? space(61)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type Stud.sha
return
*
Proc OutfPISP
SELE KREDIT
SET FILTER TO '*'+VU $ '*916*917*918'
GO TOP
if eof()
   set filter to
   return
endif
set rela to tn into super
if select('suder')=0
   SELE 0
   USE SUDER index suderind
endif
if select('bank7')=0
   SELE 0
   USE BANK7
endif
IT=0
sele kredit
do ShapPISP
DO WHILE .NOT. EOF()
    SELE SUDER
    seek KREDIT->VU
    SELE KREDIT
    if prow()>=Page_Size .or. row()>=Page_Size
       do EndPage with 'ShapPISP'
    endif
    ? '  '+NC+'  '+TN+'  '+super->FIO+STR(SVO/100,8,2)
    ? '         '+VU+' '+SUDER->NAIM+'                '+ND+STR(SO/100,13,2);
    +STR(SKU/100,12,2)
    IT=IT+SKU
    SELE BANK7
    if count_prn=1
       APPE BLANK
       REPL BANK7->VU WITH KREDIT->VU,BANK7->KO WITH KREDIT->KO,;
            BANK7->SU WITH KREDIT->SKU,BANK7->TN WITH KREDIT->TN,;
            BANK7->NC WITH KREDIT->NC
    endif
    SELE KREDIT
    SKIP
ENDDO
if prow()>=Page_Size .or. row()>=Page_Size
   do EndPage with 'ShapPISP'
endif
? ''
? ' ‚α¥£® ―® ―ΰ¥¤―ΰ¨οβ¨ξ                                         '+;
STR(IT/100,15,2)
Do Zaversh with 'ShapPISP',it
set rela to
SET FILTER TO
SET PRINT OFF
sele suder
use
sele bank7
use
RETURN
*
proc ShapPISP
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                               ‚ … „    ‘ ’    '
   ? '                 “„…†€H‰  — ‘‹H’…‹H› „K“…H’€ '
   do EndShap with 1
endif
? space(69)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type pisp.sha
return
*
proc OutfKVAR
SELE KREDIT
SET FILTER TO '*'+VU $ '*926*928'
GO TOP
if eof()
   set filter to
   return
endif
set rela to tn into super
SELE 0
USE DOPSVED
sele sorg
loca for koo==dopsved->ukogju
gjunaim=naim
gjurs  =rs
gjuob  =ob
if select('ssp')=0
   sele 0
   use ssp
endif
SELE 0
USE SDOMADR
sele kredit
do ShapKVAR
IT=0
DO WHILE .NOT. EOF()
 ko_kred=kredit->ko
 SELE SORG
 LOCA FOR KOO=KO_kred
 SELE KREDIT
 IT2=0
 if prow()+3>=Page_Size .or. row()+3>=Page_Size
    do EndPage with 'ShapKVAR'
 endif
 ? '         '+KO+'   '+SORG->NAIM
 DO WHILE KO=ko_kred .AND. .NOT. EOF()
  if prow()>=Page_Size .or. row()>=Page_Size
     do EndPage with 'ShapKVAR'
  endif
  ? '  '+NC+'   '+TN+'   '+super->FIO
  SELE SDOMADR
  LOCA FOR TN=KREDIT->TN
  SELE KREDIT
  ? '         '+left(SDOMADR->UL,20)+'        '+SDOMADR->DOM+;
    '     '+SDOMADR->KV+'     '+ND+STR(SKU/100,12,2)
  IT2=IT2+SKU
  SKIP
 ENDDO
 if prow()>Page_Size .or. row()>Page_Size
    do EndPage with 'ShapKVAR'
 endif
 ? ''
 ? ' β®£® ―® '+SORG->NAIM+'                           '+;
 STR(IT2/100,15,2)
 ? ''
 IT=IT+IT2
 IF .NOT. EOF()
  SKIP -1
 ELSE
  GO BOTTOM
 ENDIF
 if count_prn=1
    SELE BANK
    APPE BLANK
    REPL BANK->VU WITH KREDIT->VU,BANK->KO WITH KREDIT->KO,;
         BANK->SU WITH IT2
 endif
 SELE KREDIT
 SKIP
ENDDO
if prow()+3>=Page_Size .or. row()+3>=Page_Size
   do EndPage with 'ShapKVAR'
endif
? ' ‚α¥£®                                                        '+;
STR(IT/100,15,2)
do DaDoPo with 'ShapKVAR'
SET FILTER TO
set rela to
SELE DOPSVED
USE
SELE SDOMADR
USE
sele ssp
use
RETURN
*
proc ShapKVAR
do ShapTab with 'GJU','K‚€’“  K“H€‹H›… “‘‹“ƒ ',69,'kvar'
return
*
proc ShapTab
para pref,type_t,lng,shap_n
@ 0,0 say ''
if Page_Nom=1
   ? '                           ’ €  “ ‹  ƒ  €   €  '
   ? '                 H€ ……—‘‹…H… ‹€’…†…‰ ‡€ '+type_t
   ? '                    ‡ ‡€€’H‰ ‹€’› €—•  ‘‹“†€™• '
   ? '                                  '+NAME_PRED
   ? '                                  '+m->&pref.NAIM
   ? '                           ‡€ '+Tek_M_N+' '+Tek_God
   ? '                          €‘—…’H›‰ ‘—…’ '+m->&pref.RS
   ? '                          ‚ '+m->&pref.OB
endif
? space(lng)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type &shap_n..sha
return
*
proc OutfSVET
SELE KREDIT
SET FILTER TO '*'+VU $ '*927*929'
GO TOP
if eof()
   set filter to
   return
endif
set rela to tn into super
SELE 0
USE DOPSVED
sele sorg
loca for koo==dopsved->ukosv
svnaim=naim
svrs  =rs
svob  =ob
if select('ssp')=0
   sele 0
   use ssp
endif
SELE 0
USE SDOMADR
sele kredit
do ShapSVET
IT=0
DO WHILE .NOT. EOF()
 SELE SSP
 LOCA FOR KSP=KREDIT->NC
 SELE KREDIT
 IT2=0
 if prow()>=Page_Size .or. row()>=Page_Size
    do EndPage with 'ShapSVET'
 endif
 ? '  '+NC+'  '+SSP->NBRIG
 DO WHILE NC=SSP->KSP .AND. .NOT. EOF()
  SELE SDOMADR
  LOCA FOR TN=KREDIT->TN
  SELE KREDIT
  if prow()>=Page_Size .or. row()>=Page_Size
     do EndPage with 'ShapSVET'
  endif
  ? ' '+TN+' '+super->FIO+' '+left(SDOMADR->UL,20)+;
    ' '+SDOMADR->DOM+' '+SDOMADR->KV+' '+ND+STR(SKU/100,9,2)
  IT2=IT2+SKU
  SKIP
 ENDDO
 if prow()+3>=Page_Size .or. row()+3>=Page_Size
    do EndPage with 'ShapSVET'
 endif
 ? ''
 ? ' β®£®                                                          '+;
 STR(IT2/100,15,2)
 ? ''
 IT=IT+IT2
ENDDO
if prow()+2>=Page_Size .or. row()+2>=Page_Size
   do EndPage with 'ShapSVET'
endif
? ' ‚α¥£®                                                          '+;
STR(IT/100,15,2)
? ''
IF .NOT. EOF()
 SKIP -1
ELSE
 GO BOTTOM
ENDIF
if count_prn=1
   SELE BANK
   APPE BLANK
   REPL BANK->VU WITH KREDIT->VU,BANK->KO WITH KREDIT->KO,;
        BANK->SU WITH IT
endif
SELE KREDIT
SET FILTER TO
set rela to
do DaDoPo with 'ShapSVET'
SELE DOPSVED
USE
SELE SDOMADR
USE
sele ssp
use
RETURN
*
proc ShapSvet
do ShapTab with 'SV','‹…K’H…ƒ ',69,'svet'
return
*
proc OutfTEL
SELE KREDIT
SET FILTER TO VU='940'
GO TOP
if eof()
   set filter to
   return
endif
set rela to tn into super
SELE 0
USE DOPSVED
sele sorg
loca for koo==dopsved->ukotel
telnaim=naim
telrs  =rs
telob  =ob
if select('ssp')=0
   sele 0
   use ssp
endif
SELE 0
USE SDOMADR
sele kredit
do ShapTEL
IT=0
DO WHILE .NOT. EOF()
 SELE SSP
 LOCA FOR KSP=KREDIT->NC
 SELE KREDIT
 IT2=0
 if prow()>=Page_Size .or. row()>=Page_Size
    do EndPage with 'ShapTEL'
 endif
 ? '  '+NC+'  '+SSP->NBRIG
 DO WHILE NC=SSP->KSP .AND. .NOT. EOF()
  SELE SDOMADR
  LOCA FOR TN=KREDIT->TN
  SELE KREDIT
  if prow()>=Page_Size .or. row()>=Page_Size
     do EndPage with 'ShapTEL'
  endif
  ? ' '+TN+' '+super->FIO+' '+left(SDOMADR->UL,20)+' '+SDOMADR->DOM+;
    ' '+SDOMADR->KV+' '+ND+STR(SKU/100,9,2)
  IT2=IT2+SKU
  SKIP
 ENDDO
 if prow()+3>=Page_Size .or. row()+3>=Page_Size
    do EndPage with 'ShapTEL'
 endif
 ? ''
 ? ' β®£®                                                          '+;
 STR(IT2/100,15,2)
 ? ''
 IT=IT+IT2
ENDDO
if prow()+2>=Page_Size .or. row()+2>=Page_Size
   do EndPage with 'ShapTEL'
endif
? ' ‚α¥£®                                                          '+;
STR(IT/100,15,2)
? ''
IF .NOT. EOF()
 SKIP -1
ELSE
 GO BOTTOM
ENDIF
if count_prn=1
   SELE BANK
   APPE BLANK
   REPL BANK->VU WITH KREDIT->VU,BANK->KO WITH KREDIT->KO,;
        BANK->SU WITH IT
endif
SELE KREDIT
SET FILTER TO
set rela to
do DaDoPo with 'ShapTEL'
SELE DOPSVED
USE
SELE SDOMADR
USE
sele ssp
use
RETURN
*
proc ShapTEL
do ShapTab with 'TEL','’…‹…”H',69,'tel'
return
*
proc OutfRees
if select('aliment')=0
  SELE 0
  USE ALIMENT
else
  sele aliment
endif
go top
if eof()
   use
   return
endif
index on ksp to aliment
if select('SSP')=0
   sele 0
   use ssp
endif
Page_Nom=1
do ShapAlim
sele aliment
go top
SUMPEN=0
TABN=space(len(tn))
kodp=space(len(ksp))
stor 0 to itrr1,itrr2,itrr3
DO WHILE .NOT.EOF()
   if kodp<>ksp
      kodp=ksp
      if prow()>=Page_Size .or. row()>=Page_Size
        do EndPage with 'ShapAlim'
      endif
      ? ''
      sele ssp
      loca for ksp==kodp
      ? '   '+KODP+' '+nbrig
      sele aliment
      ? ''
      TABN=space(len(tn))
   endif
   IF TN<>TABN
      if prow()>=Page_Size .or. row()>=Page_Size
        do EndPage with 'ShapAlim'
      endif
      sele super
      seek aliment->tn
      sele aliment
      ? space(len(ksp)+8)+super->fio
   ENDIF
   TABN=TN
   if prow()>Page_Size .or. row()>Page_Size
     do EndPage with 'ShapAlim'
   endif
   ? STR(ITS/100,11,2)+STR(IPD/100,11,2)+STR((its-ipd)/100,11,2)+' '+;
         if(sps=0,space(len(nil)),nil)+STR(USU/100,9,2)+if(uvu='919','%',' ');
         +STR(Spp/100,11,2)+if(sps<>0,str(sps/100,11,2)+STR(sku/100,12,2),'')
   ITRR2=ITRR2+Spp
   ITRR3=ITRR3+sps
   itrr1=itrr1+sku
   sele aliment
   SKIP
ENDDO
if prow()>Page_Size .or. row()>Page_Size
  do EndPage with 'ShapAlim'
endif
? ''
? ' ‚α¥£®                                        '+STR(ITRR2/100,15,2)+;
    STR(ITRR3/100,11,2)+' '+STR(ITRR1/100,11,2)
*? if(itrr2<>itrr1+itrr3,' ‚ β.η. ¤«ο ―¥ΰ¥η¨α«¥­¨ο Ά α΅¥ΰ΅ ­ª¨     '+;
*    str((itrr2-itrr1-itrr3)/100,15,2),'')
do DaDoPo with 'ShapAlim'
do EndPage
sele aliment
set rela to
set index to
sele ssp
use
SELE 0
USE SADRA
sele 0
use dopsved
STOR 0 TO IT1,IT2,IT11,IT12,IT21,IT22
Page_Nom=1
SELE ALIMENT
set filt to spp<>0    && β®«μª® ―®ηβ®Άλ¥ ―¥ΰ¥Ά®¤λ
go top
if ! eof()
 index on kpa to aliment
 go top
 do ShapREES
 old_kod=left(kpa,1)
 DO WHILE .NOT. EOF()
  SELE SADRA
  LOCA FOR KAP=ALIMENT->KPA
  SELE ALIMENT
  if val(left(kpa,1))>=6 .and. val(old_kod)<6
     if prow()>=Page_Size .or. row()>=Page_Size
        do EndPage with 'ShapREES'
     endif
     ? ''
     ? ' β®£® ―® £®ΰ®¤αª¨¬ ―®«γη β¥«ο¬  «¨¬¥­β®Ά     '+STR(IT11/100,18,2);
     +STR(IT21/100,10,2)
     ? ''
     old_kod=left(kpa,1)
  endif
  if prow()>=Page_Size .or. row()>=Page_Size
     do EndPage with 'ShapREES'
  endif
  ? '  '+NIL+'     '+KPA+'    '+SADRA->FIA+STR(Sku/100,12,2)+STR(SPS/100,10,2)
  ? '                      '+SADRA->APA
  IF VAL(KPA)<600
*   IT11=IT11+SPP
   IT11=IT11+Sku
   IT21=IT21+SPS
  ELSE
   IT12=IT12+Sku
*   IT12=IT12+SPP
   IT22=IT22+SPS
  ENDIF
  SKIP
 ENDDO
 IT1=IT11+IT12
 IT2=IT21+IT22
 if val(old_kod)<6
    if prow()>=Page_Size .or. row()>=Page_Size
       do EndPage with 'ShapREES'
    endif
    ? ''
    ? ' β®£® ―® £®ΰ®¤αª¨¬ ―®«γη β¥«ο¬  «¨¬¥­β®Ά     '+STR(IT11/100,18,2);
    +STR(IT21/100,10,2)
 endif
 if val(old_kod)>=6
    if prow()>=Page_Size .or. row()>=Page_Size
       do EndPage with 'ShapREES'
    endif
    ? ''
    ? ' β®£® ―® ¨­®£®ΰ®¤­¨¬ ―®«γη β¥«ο¬  «¨¬¥­β®Ά   '+STR(IT12/100,18,2);
    +STR(IT22/100,10,2)
 endif
 if prow()>=Page_Size .or. row()>=Page_Size
    do EndPage with 'ShapREES'
 endif
 ? ''
 ? ' ‚α¥£® ―® ―ΰ¥¤―ΰ¨οβ¨ξ                         '+STR(IT1/100,18,2);
   +STR(IT2/100,10,2)
 it=it1+it2
 do Zaversh with 'ShapREES',it1
 if count_prn=1
   SELE BANK
   APPE BLANK
   REPL BANK->VU WITH '919',BANK->KO WITH dopsved->ukopoch,;
        BANK->SU WITH IT
 endif
 do EndPage
endif
SELE aliment
SET FILT TO spp=0    && β®«μª®  «¨¬¥­βλ Ά α΅¥ΰ΅ ­ª
go top
if ! eof()
 index on ko to aliment
 go top
 old_bank=left(ko,4)
 DO WHILE .t.
  ko_kred=aliment->ko
  SELE SORG
  LOCA FOR KOO=KO_kred
  SELE aliment
  Page_Nom=1
  do ShapAlba
  IT=0
  sele aliment
  DO WHILE KO=ko_kred .AND. .NOT. EOF()
   if prow()>Page_Size .or. row()>Page_Size
     do EndPage with 'ShapAlba'
   endif
   sele sadra
   loca for kap=aliment->kpa
   sele aliment
 * ? '  '+NC+'    '+TN+' '+super->FIO+'   '+;
   ? '  '+sadra->fia+'           '+;
     left(nil+space(11),11)+STR(SKU/100,10,2)
   IT=IT+SKU
   SKIP
  ENDDO
  if prow()>=Page_Size .or. row()>=Page_Size
    do EndPage with 'ShapAlba'
  endif
  ? ''
  ? ' ‚α¥£® ―® ―ΰ¥¤―ΰ¨οβ¨ξ'+space(30)+STR(IT/100,13,2)
  ? ''
  do Zaversh with 'ShapAlba',it
  IF .NOT. EOF()
   SKIP -1
  ELSE
   GO BOTTOM
  ENDIF
  if count_prn=1
     SELE BANK
     LOCA FOR '941'=BANK->VU .AND. left(SORG->KOO,4)=left(BANK->KO,4)
     IF EOF()
      APPE BLANK
     elseif old_bank<>left(bank->ko,4)
      repl su with 0
     ENDIF
     REPL BANK->VU WITH '941',BANK->KO WITH left(aliment->KO,4),;
          BANK->SU WITH IT+bank->su
  endif
  old_bank=left(aliment->ko,4)
  SELE aliment
  SKIP
  if eof()
     exit
  endif
  do EndPage
 ENDDO
endif
set print off
set cons on
sele aliment
use
sele sadra
use
sele dopsved
use
return
proc ShapREES
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                       ……‘’ —’‚›• ……‚„‚, '
   ? '                       „€HH›• ‚ ’„…‹…H… ‘‚‡ '
   do EndShap with 1
endif
? space(65)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type rees.sha
return
*
proc ShapAlim
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                            ‚ … „    ‘ ’    '
   ? '               “„…†€H‰  ‘‹H’…‹H› ‹‘’€ (€‹…H’›) '
   do EndShap with 1
endif
? space(65)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type alim.sha
return
*
proc ShapAlba
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                          ‚ … „    ‘ ’    '
   ? '                  ……—‘‹…H €‹…’‚ ‚ ‘…€HK '
   do EndShap with 4
endif
? space(53)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type alba.sha
return
*
*
proc OutfBANK
sele 0
use sadra
SELE KREDIT
SET FILTER TO VU='930'
GO TOP
if eof()
   set filter to
   return
endif
go top
set rela to tn into super
old_bank=left(ko,4)
DO WHILE .t.
 ko_kred=kredit->ko
 SELE SORG
 LOCA FOR KOO=ko_kred
 SELE KREDIT
 Page_Nom=1
 do ShapBANK
 IT=0
 sele kredit
 DO WHILE KO=ko_kred .AND. .NOT. EOF()
  if prow()>Page_Size .or. row()>Page_Size
    do EndPage with 'ShapBANK'
  endif
  if len(trim(tn))<=len(sadra->kap)
     sele sadra
     loca for kredit->tn=kap
     sele kredit
     sebe=.f.
  else
     sebe=.t.
  endif
  ? '  '+NC+'    '+TN+' '+;
    if(sebe,;
       super->FIO+space(10),;
       left(sadra->FIA+space(40),len(super->fio)+10);
      )+'   '+;
    left(ND+space(12),12)+'  '+STR(SKU/100,10,2)
  IT=IT+SKU
  SKIP
 ENDDO
 if prow()>=Page_Size .or. row()>=Page_Size
   do EndPage with 'ShapBANK'
 endif
 ? ''
 ? ' ‚α¥£® ―® ―ΰ¥¤―ΰ¨οβ¨ξ'+space(40)+STR(IT/100,13,2)
 ? ''
 do Zaversh with 'ShapBANK',it
 IF .NOT. EOF()
  SKIP -1
 ELSE
  GO BOTTOM
 ENDIF
 if count_prn=1
    SELE BANK
    LOCA FOR KREDIT->VU=BANK->VU .AND. left(SORG->KOO,4)=left(BANK->KO,4)
    IF EOF()
     APPE BLANK
    elseif old_bank<>left(bank->ko,4)
     repl su with 0
    ENDIF
    REPL BANK->VU WITH KREDIT->VU,BANK->KO WITH left(KREDIT->KO,4),;
         BANK->SU WITH IT+bank->su
 endif
 old_bank=left(kredit->ko,4)
 SELE KREDIT
 SKIP
 if eof()
    exit
 endif
 do EndPage
ENDDO
sele kredit
SET FILTER TO
set rela to
SET PRINT OFF
set cons on
RETURN
*
proc ShapBANK
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                          ‚ … „    ‘ ’    '
   ? '                   ……—‘‹…H ‚K‹€„‚ ‚ ‘…€HK '
   do EndShap with 4
endif
? space(63)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type bank.sha
return
*
proc outfgost
SELE KREDIT
SET FILTER TO VU='920'
GO TOP
if eof()
   set filter to
   return
endif
set rela to tn into super
DO WHILE .t.
 ko_kred=kredit->ko
 SELE SORG
 LOCA FOR KOO=KO_kred
 SELE KREDIT
 IT1=0
 IT2=0
 Page_Nom=1
 do Shapgost
 DO WHILE KO=ko_kred .AND. .NOT. EOF()
  if prow()>=Page_Size .or. row()>=Page_Size
    do EndPage with 'Shapgost'
  endif
  ? ' '+TN+' '+super->FIO+'  '+ND+'  '+SUBS(HO,3,2)+'.'+SUBS(HO,1,2)+;
  STR(SO/100,9,2)+STR(SKU/100,8,2)+STR((SO-SKU)/100,9,2)+' '+;
  SUBS(KPU,3,2)+'.'+SUBS(KPU,1,2)
  IT1=IT1+SO
  IT2=IT2+SKU
  SKIP
 ENDDO
 if prow()>=Page_Size .or. row()>=Page_Size
   do EndPage with 'Shapgost'
 endif
 ? ''
 ? ' ‚α¥£® ―® ―ΰ¥¤―ΰ¨οβ¨ξ                        '+STR(IT1/100,9,2)+;
 STR(IT2/100,8,2)+STR((IT1-IT2)/100,9,2)
 do Zaversh with 'Shapgost',it2
 IF .NOT. EOF()
  SKIP -1
 ELSE
  GO BOTTOM
 ENDIF
 if count_prn=1
    SELE BANK
    APPE BLANK
    REPL BANK->VU WITH KREDIT->VU,BANK->KO WITH KREDIT->KO,;
         BANK->SU WITH IT2
 endif
 SELE KREDIT
 SKIP
 if eof()
    exit
 endif
 do EndPage
ENDDO
sele kredit
set rela to
SET FILTER TO
SET PRINT OFF
RETURN
proc Shapgost
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                               ‚ … „    ‘ ’    '
   ? '                           ……—‘‹…H ‘“ ‡€ ƒ‘‘’€• '
   do EndShap with 4
endif
? space(68)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type Kred.sha
return
