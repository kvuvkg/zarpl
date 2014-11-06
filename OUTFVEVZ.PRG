PARAMETER PRM
priv prm,per1,per2,dop,itsvon,itsum,itprvz,prvz,nu,vnu,mskp
SET COLOR TO &STDC
DO CASE
   CASE PRM='PR'
      PER1='”‘‡H›•'
      PER2='912'
      dop='chp="1"'
      mskp=14
   CASE PRM='PT'
      PER1='…‘›•'
      PER2='913'
      dop='kpss="2"'
      mskp=15
   CASE PRM='KM'
      PER1='K‘‹‘K•'
      PER2='914'
      dop='kpss="1"'
      mskp=15
   othe
      return
ENDCASE
sele super
set filter to rpv $ '12' &&.and. ys $ '01'
seek kodpod &&go top
if eof()
   set filter to
   return
endif
SELE 0
USE SVO
declare kodna[recc()], maskna[recc()]
do ReadMskM with kodna, maskna
use
Page_Nom=1
do ShapVeVz
ITSUM=0
ITPRVZ=0
PRVZ=0
sele super
DO WHILE .NOT.EOF() .and. ksp=kodpod
 sele basef
 go super->base_rec
 if .not.(ys $ '01') .or. .not. &dop
    sele super
    skip
    loop
 endif
 itsvon=0
 for vnu=1 to 12
   nu=if(vnu<10,str(vnu,1),str(vnu,2))
   if vo&nu<>' '
      j=ascan(kodna,vo&nu)
      if j>0 .and. j<=len(kodna)
         if substr(maskna[j],mskp,1)='1'
            itsvon=itsvon+svon&nu
         endif
      endif
   endif
   IF KU&NU=PER2
      PRVZ=SKU&NU
   ENDI
 next
 if prow()>Page_Size .or. row()>Page_Size
   do EndPage with 'ShapVeVz'
 endif
 ? '  '+TN+' '+super->fio+ STR(ITSVON/100,10,2)+STR(PRVZ/100,10,2)
 ITSUM=ITSUM+ITSVON
 ITPRVZ=ITPRVZ+PRVZ
 prvz=0
 itsvon=0
 sele super
 SKIP
ENDDO
? ' '
if prow()>Page_Size .or. row()>Page_Size
  do EndPage with 'ShapVeVz'
endif
? ' ‚α¥£® ―® ―®¤ΰ §¤¥«¥­¨ξ      '+STR(ITSUM/100,10,2)+STR(ITPRVZ/100,10,2)
do DaDoPo with 'ShapVeVz'
sele super
SET FILTER TO
RETURN
*
proc ShapVeVz
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '      ‚…„‘’ “„…†€ '+PER1+' ‚‡H‘‚ '
   ? '                ‡€ '+mesname+' '+Tek_God
endif
? substr(kodpod+' '+NAMEPOD+space(40),1,40)+'‹¨αβ '+substr(str(100+Page_Nom,3),2,2)+'-'+mespod
type vevz.sha
return
