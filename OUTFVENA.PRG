*OutfVeNa 07/01/89   28.2.89,09:55 call curson/off-->set curs on/off
*24/10/89 11:35
para vidvedo
priv ww
ww='                                                                             '

if .not.(vidvedo $ 'PS')
   return
endif

sele super
SET FILTER TO RPV $ '012'

seek kodpod

if eof()
   set filter to
   return
endif

SELE 0
USE SVO index svoindex
do case
   case vidvedo='P'
      do PVZP
   case vidvedo='S'
      do SDZP
endcase
set cons on
SET PRINT off
sele basef
SET FILTER TO
sele svo
use
return
*

proc PVZP

SUMI=0
sum_i=0
store 0 to d2,kop
Page_Nom=1
do ShapPVZP
do=0
ido=0
sele super

DO WHILE .NOT.EOF() .and. ksp=kodpod

 sele basef
 go super->base_rec
 if .not.(ys $ '01')
    sele super
    skip
    loop
 endif
 
 i=1+int(len(trim(vo1)+trim(vo2)+trim(vo3)+trim(vo4)+trim(vo5)+trim(vo6)+;
     trim(vo7)+trim(vo8)+trim(vo9)+trim(vo10)+trim(vo11)+trim(vo12))/len(vo1))
 if prow()+i>Page_Size .or. row()+i>Page_Size
   do EndPage with 'ShapPVZP'
 endif
       
 ? ' '+TN+' '+super->fio+' '+ STR(OKS/100,14,2)+' '
 STOR 0 TO SUM,sum2
na='     ' 
 for vnu=1 to 12
  nu=ltrim(str(vnu,2))
  IF VO&NU<>'   '
   SELE SVO
   seek basef->VO&NU
sele basef
if vo&nu<>'105'
*   SELE basef
      ? '                                  '+STR(DN&NU/100,10,2)+' '+KMN&NU+'  '+VO&NU;
      +' '+left(SVO->NAIM,10)+' '+STR(SVON&NU/100,15,2)
      STOR SUM+SVON&NU TO SUM
else
?'                                                                             '
     ww=SVO->NAIM+'        '+'                                      '+ STR(SVON&NU/100,19,2)
stor sum+svon&nu to sum
endif

  ENDIF
****************************************
do case

case vo&nu#'   '.and.KU&NU#'   '
SELE BASEF
do case
case ku&nu='909'
na='���� �� ࠡ���'
case ku&nu='910'
na='����室. ����� '
case ku&nu='912'
na='����. ������  '
case ku&nu='913'
na='���ᨮ��� 䮭�'
case ku&nu='914'
na='����c �� ����� '
case ku&nu='915'
na='������� ����� '
case ku&nu='916'
na='���� (�㡫�)  '
case ku&nu='917'
na='���� (%)      '
case ku&nu='918'
na='��ᯮ諨��     '
case ku&nu='919'
na='��������       '
case ku&nu='920'
na='�������       '
case ku&nu='921'
na='�।��         '
case ku&nu='922'
na='�ᯮ��. �����  '
case ku&nu='924'
na='�����. �믫��� '
case ku&nu='925'
na='������ � �����'
case ku&nu='926'
na='���⥦� ������'
case ku&nu='927'
na='ᢥ� (������)'
case ku&nu='930'
na='���. � ᡥࡠ��'
case ku&nu='931'
na='�����.������.��'
case ku&nu='934'
na='����,���� ���'
case ku&nu='935'
na='�����室 ���.'
case ku&nu='936'
na='�������,����'
case ku&nu='937'
na='㤥�.�� ᯥ殤'
case ku&nu='939'
na='�����.��騬'
case ku&nu='999'
na='���.㤥ঠ��� '
case ku&nu='933'
na='㤥�.�������'
case ku&nu='932'
na='���.���.��.��� '
case ku&nu='940'
na='���. �� ⥫�䮭'
case ku&nu='941'
na='���.��.���.  '
case ku&nu='942'
na='�� ��.梥⮢��.'
case ku&nu='943'
na='�� ��.஧����. '
case ku&nu='944'
na='�� ��.��⮬���.'
case ku&nu='945'
na='����� � ���    '
case ku&nu='946'
na='               '
case ku&nu='947'
na='               '
case ku&nu='948'
na='               '
case ku&nu='949'
na='               '
case ku&nu='989'
na='����� ��� ���  '
endcase

??' '+KMU&NU+' '+KU&NU+' '+na+' '+STR(SKU&NU/100,16,2)
store sum2+sku&nu to sum2

case vo&nu='   '.and.ku&nu#'   '
do case
case ku&nu='909'
na='���� �� ࠡ���'
case ku&nu='910'
na='����室. ����� '
case ku&nu='912'
na='����. ������  '
case ku&nu='913'
na='���ᨮ��� 䮭�'
case ku&nu='914'
na='����� �� ����� '
case ku&nu='915'
na='������� ����� '
case ku&nu='916'
na='���� (�㡫�)  '
case ku&nu='917'
na='���� (%)      '
case ku&nu='918'
na='��ᯮ諨��     '
case ku&nu='919'
na='��������       '
case ku&nu='920'
na='�������       '
case ku&nu='921'
na='�।��         '
case ku&nu='922'
na='�ᯮ��. �����  '
case ku&nu='924'
na='�����. �믫��� '
case ku&nu='925'
na='������ � �����'
case ku&nu='926'
na='���⥦� ������'
case ku&nu='927'
na='ᢥ� (������)'
case ku&nu='930'
na='���. � ᡥࡠ��'
case ku&nu='931'
na='�����.������.��'
case ku&nu='934'
na='����,���� ���'
case ku&nu='935'
na='�����室 ���.'
case ku&nu='936'
na='�������,����'
case ku&nu='937'
na='㤥�.�� ᯥ殤'
case ku&nu='939'
na='�����.��騬'
case ku&nu='999'
na='���.㤥ঠ��� '
case ku&nu='933'
na='㤥�.�������'
case ku&nu='932'
na='���.���.��.��� '
case ku&nu='940'
na='���. �� ⥫�䮭'
case ku&nu='941'
na='���.��.���.  '
case ku&nu='942'
na='�� ��.梥⮢��.'
case ku&nu='943'
na='�� ��.஧����. '
case ku&nu='944'
na='�� ��.��⮬���.'
case ku&nu='945'
na='����� � ����   '
case ku&nu='946'
na='               '
case ku&nu='947'
na='               '
case ku&nu='948'
na='               '
case ku&nu='949'
na='               '
case ku&nu='989'
na='������ ��� ���'
endcase
sele basef
?'                                                                             '
??' '+kmu&nu+' '+KU&NU+' '+na+' '+STR(SKU&NU/100,16,2)
store sum2+sku&nu to sum2
endcase

****************************************
 next
 SELE basef
 
 if prow()>Page_Size .or. row()>Page_Size
   do EndPage with 'ShapPVZP'
 endif
 ?ww
ww='                                                                             '                   
 ? '������� ��諮�� �����                                              ';
 +str(kpm/100,11,2) 
 ? '�⮣� �� ⠡��쭮�� ������:                                  ';
 +STR((kpm+SUM)/100,19,2)+'                     '+str(sum2/100,19,2)        
kop=kop+kpm
do=kpm+sum-sum2
ido=if(ys='0',do/100,do)

  if do<0
 ? '���� �� ࠡ���騬                                                  ';
 +'         '+     '                        '+str(-do/100,19,2)
   else
 ? '� �뤠�                                                         ';
 +'         '+     '                            '+str(ido,19,2)
d2=d2+ido
 ? '���⮪                                                             ';
 +'         '+     '                         '+str(do/100 - ido,19,2)
   endif

?''
 SUMI=SUMI+SUM
 sum_i=sum_i+sum2

 sele super &&basef
 SKIP
ENDDO
sele basef

if prow()>Page_Size .or. row()>Page_Size
  do EndPage
endif
   
? '�ᥣ� �� ���ࠧ�������                                       ';
+STR((kop+SUMI)/100,19,2)+'                     '+str(sum_i/100,19,2)         
? "� �뤠�                                                          ";
+"              "+  "                     "+str(d2,19,2)
do DaDoPo with 'ShapPVZP'
RETURN
*
proc ShapPVZP
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '                    ��������� H������H�� � ��������� �������H�� ����� '
   ? '                                    �� '+mesname+' '+Tek_God
endif
? substr(kodpod+' '+NAMEPOD+space(69),1,69)+'���� '+substr(str(100+Page_Nom,3),2,2)+'-'+mespod
type pvzp.sha
return
*
proc SDZP
ZOSI=0
sele super

DO WHILE .NOT.EOF() .and. ksp=kodpod

 sele basef
 go super->base_rec
 if .not.(ys $ '01')
    sele super
    skip
    loop
 endif
 ZOS=0
 for vnu=1 to 12
  nu=ltrim(str(vnu,2))
  IF '*'+VO&NU $ '*102*105*101*103'
   ZOS=zos+svon&nu
  ENDIF
 next
 ZOSI=ZOSI+ZOS
 sele super
 SKIP
ENDDO
set cons on
if zosi=0
   do ErrorTxt with ' �� ���ࠧ������� �� ���᫥�� ᤥ�쭮� �/� !!! '
   set print off
   tp_out=0
   return
endif
clear
@ 1,0 say namepod+' '+mespod
@ 2,0 say ''
? '                              -----------------------'
? '                              |   �����    |K������.|'
? '                              |�����H�� �/�|������- |'
? '                              | (��.���.) |����H�� |'
? '                              -----------------------'
SSDZP=0
set color to &ecol
@ 24,0
@ 24,3 say '������ �㬬� ᤥ�쭮� �/� ��� {Esc}/{����}-�⪠�'
set color to &stdc
keyboard ''
set curs on
@ 8,35 GET SSDZP PICT "####.##"
READ
@ 24,0
set curs off
if lastkey()=27
   return
endif
KSDZP=SSDZP*100/ZOSI
@ 8,0
@ 8,0 say ''
? '                              '+str(SSDZP,8,2)+'    '+STR(round(KSDZP,3),6,3)
if tp_out=1
   set cons off
else
   clear
endif
PageNom=1
do ShapSDZP
SELE super
seek kodpod
SUMI=0
SZPI=0
VSEGOI=0

declare allna[12]
l_fio=len(super->fio)

DO WHILE .NOT.EOF() .and. ksp=kodpod
 
 sele basef
 go super->base_rec
 if .not. (ys $ '01')
    sele super
    skip
    loop
 endif
 
 i=1+int(len(trim(vo1)+trim(vo2)+trim(vo3)+trim(vo4)+trim(vo5)+trim(vo6)+;
     trim(vo7)+trim(vo8)+trim(vo9)+trim(vo10)+trim(vo11)+trim(vo12))/len(vo1))
 if prow()+i>Page_Size .or. row()+i>Page_Size
   do EndPage with 'ShapSDZP'
 endif
 
 ? ' '+TN+'  '+super->FIO+' '+ STR(OKS/100,9,2)
 STOR 0 TO SUM
 zos=0
 
 for vnu=1 to 12
  nu=ltrim(str(vnu,2))
  IF VO&NU<>'   '
   IF '*'+VO&NU $ '*102*105*101*103'
    ZOS=zos+svon&nu
   ELSE
    SELE SVO
    seek basef->VO&NU
    SELE basef
    
    if prow()>Page_Size .or. row()>Page_Size
      do EndPage with 'ShapSDZP'
    endif
       
    ? ' '+VO&NU+'  '+substr(SVO->NAIM+space(l_fio),1,l_fio)+;
      '   '+STR(SVON&NU/100,10,2)
    STOR SUM+SVON&NU TO SUM
   ENDIF
  ENDIF
 next
 
 SELE basef
 SZP=ZOS*KSDZP

 w='*'+vo1+'*'+vo2+'*'+vo3+'*'+vo4+'*'+vo5+'*'+vo6+;
   '*'+vo7+'*'+vo8+'*'+vo9+'*'+vo10+'*'+vo11+'*'+vo12
 w1='105'
 i=at('*101',w)
 if i=0
    i=at('*101',w)
    if i=0
       i=at('*102',w)
       if i=0
          i=at('*103',w)
       endif
    endif
 endif
 if i<>0
    w1=substr(w,(i-1)*4+2,3)
 endif
 i=AddNewK(w1,'105',szp,.t.)

 VSEGO=SUM+SZP
 
 if prow()>Page_Size .or. row()>Page_Size
   do EndPage with 'ShapSDZP'
 endif
    
 ? ' �⮣� �� ⠡. ������:        '+STR(SUM/100,9,2)+' '+STR(ZOS/100,10,2)+;
 STR(SZP/100,10,2)+STR(VSEGO/100,11,2)
 SUMI=SUMI+SUM
 SZPI=SZPI+SZP
 VSEGOI=VSEGOI+VSEGO
 SELE super &&basef
 SKIP
ENDDO
SELE basef

if prow()>Page_Size .or. row()>Page_Size
  do EndPage with 'ShapSDZP'
endif
   
? ' �ᥣ� �� ���ࠧ�������       '+STR(SUMI/100,9,2)+' '+STR(ZOSI/100,10,2)+;
STR(SZPI/100,10,2)+STR(VSEGOI/100,11,2)
do DaDoPo with 'ShapSDZP'
RETURN
*
proc ShapSDZP
@ 0,0 say ''
if Page_Nom=1
   ? name_pred
   ? '        ��������� H������H�� �����H�� �������H�� ����� '
   ? '                  �� '+mesname+' '+Tek_God
endif
? substr(kodpod+' '+NAMEPOD+space(62),1,62)+'���� '+substr(str(100+Page_Nom,3),2,2)+'-'+mespod
type sdzp.sha
return
*
