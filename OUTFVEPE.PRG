SELE basef
SET FILTER TO VP='1' .AND. YS = '0' .AND. RPV $ '12' .AND. val(PB)<>0
GO TOP
if eof()
   set filter to
   return
endif
set filter to
SET COLOR TO &STDC
SELE 0
USE SRD
GO VAL(MESPOD)
SELE 0
USE SVOZR
do ShapPENS
SUMPEN=0
sele super
set filter to rpv $ '12' &&.and. ys='0'
seek kodpod &&go top
DO WHILE .NOT. EOF() .and. ksp=kodpod
 sele basef
 go super->base_rec
 if VP<>'1' .or. YS <> '0' .or. val(PB)=0
    sele super
    skip
    loop
 endif
 if prow()>=Page_Size .or. row()>=Page_Size
   do EndPage with 'ShapPENS'
 endif
 sele basef
 ? '                               '+TN+'  '+super->FIO
 SVONI=SVON1+SVON2+SVON3+SVON4+SVON5+SVON6+SVON7+SVON8+SVON9+SVON10+SVON11+SVON12
 SELE SVOZR
 LOCA FOR KKVP=basef->PB
 SELE basef
 RR1=SPEN*SVOZR->PRVYP/100
 rr4=if(type('basef->maxpen')='N',basef->maxpen,SVOZR->MAXPZ*100)
 if rr4=0
    rr4=SVOZR->MAXPZ*100
 endif
 if rr4=0
    rr3=rr1
 else
    RR2=SVONI+RR1
    IF RR2<=rr4
       RR3=RR1
    ELSE
       rr3=max(mpen,rr4-svoni)         &&&&&&&RR3=MAX(RR1-RR2,MPEN)
    ENDIF
 endif
 ? '  '+KODPOD+'  '+NOMP+'   '+VP+'     '+PB+'   '+STR(RR1/100,9,2)+' ';
 +SRD->NAIM+STR(SVONI/100,8,2)+STR((RR1+SVONI)/100,8,2)+STR(RR3/100,8,2)
 SUMPEN=SUMPEN+RR3
 sele super
 SKIP
ENDDO
if prow()>=Page_Size .or. row()>=Page_Size
  do EndPage with 'ShapPENS'
endif
? ' '
? ' ‚á¥£® ¯® ¯®¤à §¤¥«¥­¨î                                       '+;
STR(SUMPEN/100,9,2)
do DaDoPo with 'ShapVePe'
sele srd
use
sele svozr
use
sele basef
set filter to
sele super
set filter to
RETURN
proc ShapPENS
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                                ‚ … „  Œ  ‘ ’ œ   '
   ? '                 H€ ‚›‹€’“ …H‘ˆ‰ €’€™ˆŒ …H‘ˆH…€Œ ‡€ ‘—…’ '
   ? '                     ƒ‘“„€‘’‚…HHƒ ‘–ˆ€‹œHƒ ‘’€•‚€HˆŸ   '
   ? '                           ‡€ '+mesname+' '+Tek_God
endif
? substr(kodpod+' '+NAMEPOD+space(70),1,70)+'‹¨áâ '+;
  substr(str(100+Page_Nom,3),2,2)+'-'+mespod
type pens.sha
return
