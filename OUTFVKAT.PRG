SELE 0
use svod
if eof()
   use
   return
endif
do PrepVKAT
do ShapVKAT
sele svod
INDEX ON KR+VO TO SVODIN
GO TOP
STOR 0 TO IT1,IT2,IT3
DO WHILE VAL(KR)<40 .AND. .NOT. EOF()
 DO DOPKTG
ENDDO
ITOG=IT1+IT2+IT3
*if prow()>=Page_Size .or. row()>=Page_Size
*  do EndPage with 'ShapVKAT'
*endif
? ' '
*? 'ˆâ®£® ¯® ¯à®¬.¯à®¨§.¯¥àá®­                '+STR(ITOG/100,10,2)+;
*STR(IT1/100,10,2)+STR(IT2/100,9,2)+STR(IT3/100,7,2)
? ' '
STOR 0 TO IT1,IT2,IT3,ITOG
DO WHILE VAL(KR)<80 .AND. .NOT. EOF()
 DO DOPKTG
ENDDO
ITOG=IT1+IT2+IT3
*if prow()>=Page_Size .or. row()>=Page_Size
*  do EndPage with 'ShapVKAT'
*endif
*? ' '
*? 'ˆâ®£® ¯® ­¥¯à®¬. ¯¥àá®­ «ã                '+STR(ITOG/100,10,2)+;
*STR(IT1/100,10,2)+STR(IT2/100,9,2)+STR(IT3/100,7,2)
? ' '
STOR 0 TO IT1,IT2,IT3,ITOG
DO WHILE .NOT. EOF()
 DO DOPKTG
ENDDO
ITOG=IT1+IT2+IT3
*if prow()>=Page_Size .or. row()>=Page_Size
*  do EndPage with 'ShapVKAT'
*endif
*? ' '
*? 'ˆâ®£® ¯® ­¥á¯¨á®ç. á®áâ ¢ã                '+STR(ITOG/100,10,2)+;
*STR(IT1/100,10,2)+STR(IT2/100,9,2)+STR(IT3/100,7,2)
SUM FMPVS,FMPB,FMPT,FZPVS,FZPB,FZPT,PRVS,PRB,PRT;
 TO FMV,  FMB, FMT, FZV,  FZB, FZT, PRV, PRB,PRT
VS=FMV+FZV+PRV
FMP=FMV-(FMB+FMT)
FZP=FZV-(FZB+FZT)
PRP=PRV-(PRB+PRT)
VSP=FMP+FZP+PRV
VST=FMT+FZT+PRT
VSB=FMB+FZB+PRB
*if prow()>=Page_Size-6 .or. row()>=Page_Size-6
*  do EndPage with 'ShapVKAT'
*endif
? ' '
*? '‚á¥£® ­ ç¨á«¥­®                           '+STR(VS/100,10,2)+;
*STR(FzV/100,10,2)+STR(FmV/100,9,2)+STR(PRV/100,7,2)
*? '¢ â.ç. §  ¯à®è«.¬-æ                       '+STR(VSP/100,10,2)+;
*STR(FzP/100,10,2)+STR(FmP/100,9,2)+STR(PRP/100,7,2)
*? '      §  â¥ªãé¨© ¬-æ                      '+STR(VST/100,10,2)+;
*STR(FzT/100,10,2)+STR(FmT/100,9,2)+STR(PRT/100,7,2)
*? '      ¨â®£®                          '+STR((VS-VSB)/100,15,2)+;
*STR((FzV-FzB)/100,10,2)+STR((FmV-FmB)/100,9,2)+STR((PRP+PRT)/100,7,2)
*? '      ¢ áç¥â ¡ã¤ãé.¬-æ                    '+STR(VSB/100,10,2)+;
*STR(FzB/100,10,2)+STR(FmB/100,9,2)+STR(PRB/100,7,2)
do DaDoPo with 'ShapVKAT'
sele svod
use
erase svodin.ntx
sele skr
use
sele svo
use
sele vrempere
use
RETURN
*
proc DopKTG
SELE SKR
LOCA FOR SVOD->KR=KKP
SELE SVOD
*if prow()>=Page_Size .or. row()>=Page_Size
*  do EndPage with 'ShapVKAT'
*endif
? '  '+KR+'  '+SKR->NAIM
store 0 to n1
DO WHILE KR=SKR->KKP .AND. .NOT. EOF()
   SELE SVO
   seek svod->vo  &&LOCA FOR SVOD->VO=KVOY
   SELE SVOD
   VS=FMPVS+FZPVS+PRVS
*   if prow()>Page_Size .or. row()>Page_Size
 *    do EndPage with 'ShapVKAT'
*   endif
   ? ' '+VO+'  '+SVO->NAIM+'     '+STR(DN/100,6)+STR(CHS/100,10,2)+;
   STR(VS/100,13,2)  
*   +STR(FzPVS/100,10,2)+STR(FmPVS/100,9,2)+STR(PRVS/100,7,2)
   n1=n1+vs
   IT1=IT1+FzPVS
   IT2=IT2+FmPVS
   IT3=IT3+PRVS
   SKIP
ENDDO
? '¨â®£®    '+str(n1/100,15,2)
RETURN
*
proc ShapVKAT
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '              ‘‚Ž„H€Ÿ ‚…„ŽŒŽ‘’œ €‘…„…‹…HˆŸ ‡€€Ž’HŽ‰ '
   ? '                  ‹€’›  Ž K€’…ƒŽˆŸŒ ˆ ‚ˆ„€Œ Ž‹€’  '
   ? '                         ‡€ '+Tek_M_N+' '+Tek_God
endif
? space(69)+'‹¨áâ '+substr(str(100+Page_Nom,3),2,2)+'-'+Tek_Mes_P
type VKAT.sha
return
*
proc PrepVKAT
if select('skr')=0
   SELE 0
   USE SKR
endif
if select('svo')=0
   SELE 0
   USE svo index svoindex
endif
if select('vrempere')=0
   sele 0
   use vrempere
endif
if select('svod')=0
   sele 0
   use svod
   zap
endif
return
*
proc VremVKAT
para kodp
SELE VREMPERE
set filter to o_ksp=kodp
go top
DO WHILE .NOT.EOF()
 SELE SVO
 seek vrempere->o_vo  &&LOCA FOR VREMPERE->O_VO=KVOY
 SELE BASEF
 GO VREMPERE->O_BASE_R
 SELE SVOD
 LOCA FOR BASEF->KR=SVOD->KR .AND. VREMPERE->O_VO=SVOD->VO
 IF (VREMPERE->O_VO>'320'.AND.VREMPERE->O_VO<'331').OR.VREMPERE->O_VO='356'
  REPL CHS WITH CHS-VREMPERE->O_DN
 ELSE
  IF VREMPERE->O_VO>'200' .AND. VREMPERE->O_VO<'300'
   REPL DN WITH DN-VREMPERE->O_DN
  ELSE
   IF BASEF->PTS='2'
    REPL CHS WITH CHS-VREMPERE->O_DN
   ELSE
    REPL DN WITH DN-VREMPERE->O_DN
   ENDI
  ENDI
 ENDI
 SELE SVOD
 DO CASE
  CASE SUBS(SVO->ZMO,1,1)='1'
   REPL FZPVS WITH FZPVS-VREMPERE->O_SVON
   DO CASE
    CASE .NOT. (VAL(MESPOD)-VAL(VREMPERE->O_KM)<9.AND.VAL(MESPOD)-VAL(VREMPERE->O_KM)>=0 ;
    .OR.VAL(MESPOD)-VAL(VREMPERE->O_KM)<-3)
     REPL FZPB WITH FZPB-VREMPERE->O_SVON
    CASE MESPOD=VREMPERE->O_KM
     REPL FZPT WITH FZPT-VREMPERE->O_SVON
   ENDCASE
  CASE SUBS(SVO->ZMO,2,1)='1'
   REPL FMPVS WITH FMPVS-VREMPERE->O_SVON
   DO CASE
    CASE .NOT. (VAL(MESPOD)-VAL(VREMPERE->O_KM)<9.AND.VAL(MESPOD)-VAL(VREMPERE->O_KM)>=0 ;
    .OR.VAL(MESPOD)-VAL(VREMPERE->O_KM)<-3)
     REPL FMPB WITH FMPB-VREMPERE->o_svon
    CASE MESPOD=VREMPERE->O_KM
     REPL FMPT WITH FMPT-VREMPERE->o_svon
   ENDCASE
  OTHER
   REPL PRVS WITH PRVS-VREMPERE->O_SVON
   DO CASE
    CASE .NOT. (VAL(MESPOD)-VAL(VREMPERE->O_KM)<9.AND.;
               (VAL(MESPOD)-VAL(VREMPERE->O_KM))>=0 ;
           .OR.VAL(MESPOD)-VAL(VREMPERE->O_KM)<-3)
     REPL PRB WITH PRB-VREMPERE->O_SVON
    CASE MESPOD=VREMPERE->O_KM
     REPL PRT WITH PRT-VREMPERE->O_SVON
   ENDCASE
 ENDCASE
 SELE VREMPERE
 SKIP
ENDDO
return
*
proc RecoVKAT
priv vnu,nnu,base_vo,base_svon
for vnu=1 to 12
  nnu=ltrim(str(vnu,2))
  base_vo=basef->vo&nnu
  if val(base_vo)<>0
     SELE SVO
     seek base_vo  &&LOCA FOR base_vo=KVOY
     SELE SVOD
     LOCA FOR BASEF->KR=SVOD->KR .AND. base_vo=SVOD->VO
     IF EOF()
      APPE BLANK
      REPL KR WITH BASEF->KR,VO WITH base_vo
     ENDIF
     IF (base_vo>'320'.AND.base_vo<'331').OR.base_vo='356'
      REPL CHS WITH CHS+BASEF->DN&NNU
     ELSE
      IF base_vo>'200' .AND. base_vo<'300'
       REPL DN WITH DN+BASEF->DN&NNU
      ELSE
       IF BASEF->PTS='2'
        REPL CHS WITH CHS+BASEF->DN&NNU
       ELSE
        REPL DN WITH DN+BASEF->DN&NNU
       ENDI
      ENDI
     ENDI
     SELE SVOD
     base_svon=BASEF->SVON&NNU
     DO CASE
      CASE SUBS(SVO->ZMO,1,1)='1'
       REPL FZPVS WITH FZPVS+base_svon
       DO CASE
        CASE .NOT. (VAL(MESPOD)-VAL(BASEF->KMN&NNU)<9.AND.VAL(MESPOD)-VAL(BASEF->KMN&NNU)>=0 ;
        .OR.VAL(MESPOD)-VAL(BASEF->KMN&NNU)<-3)
         REPL FZPB WITH FZPB+base_svon
        CASE MESPOD=BASEF->KMN&NNU
         REPL FZPT WITH FZPT+base_svon
       ENDCASE
      CASE SUBS(SVO->ZMO,2,1)='1'
       REPL FMPVS WITH FMPVS+base_svon
       DO CASE
        CASE .NOT. (VAL(MESPOD)-VAL(BASEF->KMN&NNU)<9.AND.VAL(MESPOD)-VAL(BASEF->KMN&NNU)>=0 ;
        .OR.VAL(MESPOD)-VAL(BASEF->KMN&NNU)<-3)
         REPL FMPB WITH FMPB+base_svon
        CASE MESPOD=BASEF->KMN&NNU
         REPL FMPT WITH FMPT+base_svon
       ENDCASE
      OTHER
       REPL PRVS WITH PRVS+base_svon
       DO CASE
        CASE .NOT. (VAL(MESPOD)-VAL(BASEF->KMN&NNU)<9.AND.VAL(MESPOD)-VAL(BASEF->KMN&NNU)>=0 ;
        .OR.VAL(MESPOD)-VAL(BASEF->KMN&NNU)<-3)
         REPL PRB WITH PRB+base_svon
        CASE MESPOD=BASEF->KMN&NNU
         REPL PRT WITH PRT+base_svon
       ENDCASE
     ENDCASE
  endif
next
sele basef
return
*
