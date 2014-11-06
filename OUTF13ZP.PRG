if type('basef->zarp13')<>'N'
   do ErrorTxt with 'Hет поля в базе для хранения 13-й зарплаты...'
   return
endif
setcolor(stdc)
sele super
set filter to rpv $ '01'
seek kodpod
if eof()
   set filter to
   return
endif
sele basef
IF SELECT('SSTAJ')=0
 SELE 0
 USE SSTAJ
ENDIF
RAZM=0
@ 1,1 SAY 'Введите коэффициент ' GET RAZM PICT "#.#####"
set curs on
READ
set curs off
DO Shap13ZP
IT1=0
IT2=0
sele super
DO WHILE .NOT. EOF() .and. kodpod=ksp
 sele basef
 go super->base_rec
 if ! (ys $ '01')
    sele super
    skip
    loop
 endif
 IF VAL(SUBS(PR,1,2))>50
  GD='19'
 ELSE
  GD='20'
 ENDIF
 IF VAL(SUBS(PR,3,2))>1 .AND. TEK_GOD<>SUBS(PR,1,2)
  GOD=VAL(TEK_GOD)-VAL(GD+SUBS(PR,1,2))-1
 ELSE
  GOD=VAL(TEK_GOD)-VAL(GD+SUBS(PR,1,2))
 ENDIF
 SELE SSTAJ
 LOCA FOR VAL(SP)>GOD
 SELE BASEF
 PR1=SPVZ*SSTAJ->G
 PR2=PR1*RAZM
 IF knzp<0
  PR3="'    '+STR(-knzp,4)"
 ELSEif knzp>0
  PR3="STR(knzp,4)+'    '"
 else
  pr3='"'+space(8)+'"'
 ENDIF
 PR4=PR2+PR2*knzp/100
 SELE BASEF
 IF PROW()>Page_Size .OR. ROW()>Page_Size
  DO EndPage WITH 'SHAP13zp'
 ENDIF
 ? ' '+TN+' '+SUPER->FIO+STR(SPVZ/100,9,2)+STR(SSTAJ->G,5,2)+STR(PR1/100,9,2);
 +STR(PR2/100,9,2)+&PR3+STR(PR4/100,9,2)
 IT1=IT1+PR2
 IT2=IT2+PR4
 repl zarp13 with pr4
 i=AddNewK('515','515',pr4,.t.)
 sele super
 SKIP
ENDDO
? ' '
IF PROW()>Page_Size .OR. ROW()>Page_Size
 DO EndPage WITH 'SHAP13zp'
ENDIF
? ' Итого по подразделению '+REPLI(' ',27)+STR(IT1/100,9,2)+'        ';
+STR(IT2/100,9,2)
? ' '
DO DADOPO WITH 'SHAP13ZP'
DO EndPage
sele sstaj
use
SELE super
SET FILTER TO
RETURN
*
PROC Shap13ZP
@ 0,0 SAY ''
IF Page_Nom=1
 ? NAME_PRED
 ? '                             В Е Д О М О С Т Ь    '
 ? '                        HАЧИСЛЕHИЯ ВОЗHАГРАЖДЕHИЙ ПО  '
 ? '                         ИТОГАМ РАБОТЫ ЗА '+STR(VAL(TEK_GOD)-1,4)+' ГОД '
 ? '                               '+MESNAME
 ? 'коэффициент '+STR(RAZM,9,5)
ENDIF
? left(namepod+SPACE(68),68)+'Лист '+Substr(str(100+Page_Nom,3),2,2)+'-'+mespod
type 13ZP.sha
RETURN
*
proc SpremGod
sele super
set filter to rpv $ '01'
seek kodpod
if eof()
   set filter to
   return
endif
SELE 0
USE SVO index svoindex
SELE BASEF
do ShapSpre
sele super
DO WHILE .NOT. EOF() .and. kodpod=ksp
 sele basef
 go super->base_rec
 if ! (ys $ '01')
    sele super
    skip
    loop
 endif
 ITSVO=0
 N=RECNO()
 SELE SEBAF
 NN=(N-1)*12+1
 GO NN
 end_rec=nn+12  &&(N-1)*12+13
 first_prt=.t.
 DO WHILE recno()<>end_rec .and. ! eof()
  for vnnu=1 to 12
   nnu=ltrim(str(vnnu,2))
   SELE SVO
   seek SEBAF->VO&NNU
   IF .NOT. EOF()
    SELE SEBAF
    IF SUBS(SVO->ZMO,22,1)='1'
    if prow()>Page_Size .or. row()>Page_Size
      do EndPage with 'ShapSpre'
     endif
     if first_prt
       ? '  '+BASEF->TN+'   '+super->FIO+'   '+VO&NNU+'   '+SUBS(SVO->NAIM,1,18)+;
         '    '+KMN&NNU+STR(SVON&NNU/100,11,2)
       first_prt=.f.
     else
       ? space(len(basef->tn)+len(super->fio)+8)+VO&NNU+'   '+SUBS(SVO->NAIM,1,18)+;
         '    '+KMN&NNU+STR(SVON&NNU/100,11,2)
     endif
     ITSVO=ITSVO+SEBAF->SVON&NNU
    ENDIF
   ENDIF
   SELE SEBAF
  next
  SKIP
 ENDDO
 if itsvo<>0
    if prow()>=Page_Size-1 .or. row()>=Page_Size-1
      do EndPage with 'ShapSpre'
    endif
    ? ' Итого по таб. номеру '+REPLI(' ',40)+STR(ITSVO/100,10,2)
    ? space(56)+'1/12 ='+str(round(itsvo/12.00,0)/100.00,10,2)
    ? ''
 endif
 ***repl basef->dsp with round(itsvo/12.00,0)
 SELE super   &&BASEF
 SKIP
ENDDO
sele svo
use
RETURN
*
proc ShapSpre
? REPLI('-',73)
? '|ТАБ.HОМЕР|       Ф И О       |KОД HАЧ.|  HАИМЕHОВАHИЕ  |МЕСЯЦ|  СУММА  |'
? REPLI('-',73)
return
