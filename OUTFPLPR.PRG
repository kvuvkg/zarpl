priv SprNom,SprTop,M
********* M=0
SELE 0
USE SUDER index suderind
SELE 0
USE SNP
go top
SprNom=1
SprTop=if(eof(),-999,recno())
SET RELA TO VU INTO SUDER
SELE 0
USE SORG
MES='ﭢ���  䥢ࠫ� ����   ��५�  ���     ���    ���    ������ '+;
'ᥭ���ﮪ���� �����  ������� '
MES1='ﭢ���  䥢ࠫ� ����    ��५�  ���     ���    ���    ������  '+;
'ᥭ���쮪���� �����  ������� '
L=1
do while .t.
   setcolor(stdc)
   clear
   setcolor(ucol)
   @ 1,1 say ' ����� ���⥦��� ����祭�� '
   sele snp
   xb=0
   yb=0
   do ListMenu with '',4,5,22,1,SprNom,SprTop,;
      "str(recno(),3)+'. '+VU+' '+SUDER->NAIM+' '",;
      '  - ��������  {Esc}/{����} - ����� ࠡ���  �/{����} - �롮�',;
      '','',xb,yb
   setcolor(stdc)
   @ 24,0
   if SprNom=0
      exit
   endif
   go SprTop
   skip SprNom-1
   do PrtPlPr
enddo
sele snp
use
sele suder
use
sele sorg
use
RETURN
*
proc PrtPlPr
SELE 0
IF '*'+SNP->VU $ '*916*917*918'
   USE BANK7 alias bank
ELSE
   USE BANK  alias bank
   DO CASE
    CASE '*'+SNP->VU $ '*919*941'
       A='*919*941'
    CASE '*'+SNP->VU $ '*926*928'
       A='*926*928'
    CASE '*'+SNP->VU $ '*927*929'
       A='*927*929'
    othe
       a='*'+snp->vu
   ENDCASE
   SET FILTER TO '*'+VU $ A
ENDIF
GO TOP
if eof()
   do ErrorTxt with 'H�� ������ �� '+trim(suder->naim)+'...'
   use
   return
endif
IF VAL(Tek_Mes_P)=12
   NMES=1
   GOD=VAL(Tek_God)+1
ELSE
   NMES=VAL(Tek_Mes_P)*8+1
   GOD=VAL(Tek_God)
ENDIF
NMES1=(VAL(Tek_Mes_P)-1)*8+1
DATA='  '
NPP='         '
POL1=space(31)
POL2=space(43)
POL3=space(30)
POL4=space(30)
SHAB=STUFF(SNP->SHABLON,AT('&',SNP->SHABLON),16,;
           (LEFT(SUBS(MES1,NMES1,8)+' '+Tek_God+;
                 '�. '+SPACE(16),16)))
NZP1=SUBS(SHAB,1,50)
NZP2=SUBS(SHAB,51,50)
NZP3=SUBS(SHAB,101,50)
NZP4=SUBS(SHAB,151,50)
NZP5=SUBS(SHAB,201,50)
DO CASE
   CASE LEN(pred_NAIM)<=30
      DP1=pred_NAIM+REPLIC(' ',30-LEN(pred_NAIM))
      stor space(30) to DP2,DP3
   CASE LEN(pred_NAIM)>30 .AND. LEN(pred_NAIM)<=60
       DP1=SUBS(pred_NAIM,1,30)
       DP2=SUBS(pred_NAIM,31,30)+space(60-LEN(pred_NAIM))
       DP3=space(30)
   CASE LEN(pred_NAIM)>60 .AND. LEN(pred_NAIM)<=90
       DP1=SUBS(pred_NAIM,1,30)
       DP2=SUBS(pred_NAIM,31,30)
       DP3=SUBS(pred_NAIM,61,30)+space(90-LEN(pred_NAIM))
ENDCASE
DO WHILE .NOT. EOF()
   OTV='D'
   SELE SORG
   LOCA FOR KOO=bank->KO
   if eof()
      do ErrorTxt with '��� ���� '+bank->ko+' � �ࠢ�筨�� �࣠����権 '
      sele bank
      skip
      loop
   endif
   POL1=SORG->NAIM+space(31-LEN(SORG->NAIM))
   SELE bank
   DO WHILE KO=SORG->KOO .AND. .NOT. EOF() .AND. OTV='D'
      setcolor(stdc)
      clear
      IT=SU
      v_it=it/100
      set curs on
      @ 1,1 SAY '������ :'
      @ 3,1 SAY 'H���� ���⥦���� ����祭�� ' GET NPP
      @ 5,1 SAY '��᫮                      ' GET DATA;
                 valid (val(data)>0.and.val(data)<=31)
      @ 7,1 SAY 'H����������� �����⥫�    ' GET POL1
      @ 8,29 GET POL2
      @ 9,29 GET POL3
      @ 10,29 GET POL4
      @ 12,1 SAY 'H����祭�� ���⥦� ' GET NZP1
      @ 13,21 GET NZP2
      @ 14,21 GET NZP3
      @ 15,21 GET NZP4
      @ 16,21 GET NZP5
      @ 18,1  say '�㬬� � �����...........';
              get v_it picture '########.##' valid v_it>0
      READ
      set curs off
      if lastkey()=27
         otv='N'
         exit
      endif
      it=v_it*100
      setcolor(ecol)
      @ 24,0
      @ 24,10 SAY '.....������ �����饥 ���ன�⢮ � ����.....'
      SET PRINT ON
      set cons off
      ? replicate('-',79)
      ? '                                -------------          ---------  '
      ? '          ������H�� ������H�� N | '+NPP+' |          |0401002|'
      ? '                                -------------          ---------  '
      ? '              '+DATA+' '+' '+TRIM(SUBS(MES,NMES,8))+' '+STR(GOD,4)+'�.'
      ? '                                                 �����     �����  '
      ? ' ���⥫�騪    '+DP1+' ------------------------'
      ? '               '+DP2+' |'+pred_RS+'|            |'
      ? '               '+DP3+' |         |'+STR(IT/100,12,2)+'|'
      ? '                                  ------------|��.N     |            |'
      ? ' ���� ���⥫�騪�   '+SUBS(pred_OB,1,13)+' |  '+pred_KB+'  |---------|            |'
      ? ' '+SUBS(pred_OB,14,31)+space(44-LEN(pred_OB))+;
      '  |K��        |         |            |'
      ? space(34)+'-------------         |            |'
      ? ' �����⥫�  '+POL1+'    K�����  |            |'
      ? ' '+POL2+'  ------------------------'
      ? '               '+POL3+' |'+SORG->RS+'|            |'
      ? '               '+POL4+' |��.N     |            |'
      ? '                                  ------------|---------|------------|'
      ? ' ���� �����⥫�    '+SUBS(SORG->OB,1,13)+' |  '+SORG->KB+'  |         |            |'
      ? ' '+SUBS(SORG->OB,14,31)+space(45-LEN(SORG->OB))+;
      ' |K��        |��.N     |            |'
      ? space(34)+'------------------------------------'
      ? SPACE(55)+' |���� ��     |'
      
      DLS=47
      V_ITOG=INT(IT/100)
      KOP=(IT/100-V_ITOG)*100
      STOR ' ' TO TSUM,TSUM1,TSUM2
      
*      DO SCRIPT WITH V_ITOG,TSUM,TSUM1,TSUM2,DLS
      
      IF LEN(TSUM2)=1
         TSUM1=TSUM1+'K��. '+substr(STR(KOP+100,3),2,2)
      ELSE
         TSUM2=TSUM2+'K��. '+substr(STR(KOP+100,3),2,2)
      ENDIF
      
      ? ' '+TSUM1+space(55-LEN(TSUM1))+'|��    %     |'
      ? ' '+TSUM2+space(55-LEN(TSUM2))+'|�.          |'
      ? SPACE(55)+' |�㬬� � ���.|'
      ? ' �㬬� �ய����  '+space(39)+'|            |'
      ? space(55)+' --------------'
      ? ' ��� ����祭�� ⮢��,�������� ���             19  �.|��� ���� 01 |'
      ? space(55)+' |H���.��. '+SNP->NP+'|'
      ? ' H����祭�� ���⥦�,������������ ⮢��,�믮�������     |�ப        |'
      ? ' ࠡ��,��������� ���,NN � �㬬� ⮢���� ���㬥�⮢   |N ��.       |'
      ? ' '+NZP1+'     |�����       |'
      ? ' '+NZP2+'     --------------'
      ? ' '+NZP3
      ? ' '+NZP4+'  �஢����� ������'
      ? ' '+NZP5+'             19  �'
      ? '   �.�.        ������ ������'+SPACE(23)+'������ ����� '
      ? ' '
      ? ' '
      ? ' '
      ? ' '
      set print off
      set cons on
      OTV=' '
      do AskYesNo with otv,24,'�������? ',ecol
      if lastkey()=27
         otv='N'
         exit
       endif
   ENDDO
   IF OTV='N'
      SKIP
   ENDIF
ENDDO
sele bank
use
return
