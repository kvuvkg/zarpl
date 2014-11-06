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
MES='января  февраля марта   апреля  мая     июня    июля    августа '+;
'сентябряоктября ноября  декабря '
MES1='январь  февраль март    апрель  май     июнь    июль    август  '+;
'сентябрьоктябрь ноябрь  декабрь '
L=1
do while .t.
   setcolor(stdc)
   clear
   setcolor(ucol)
   @ 1,1 say ' Печать платежных поручений '
   sele snp
   xb=0
   yb=0
   do ListMenu with '',4,5,22,1,SprNom,SprTop,;
      "str(recno(),3)+'. '+VU+' '+SUDER->NAIM+' '",;
      '  - движение  {Esc}/{Ключ} - конец работы  ┘/{Ввод} - выбор',;
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
   do ErrorTxt with 'Hет данных по '+trim(suder->naim)+'...'
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
                 'г. '+SPACE(16),16)))
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
      do ErrorTxt with 'Нет кода '+bank->ko+' в справочнике организаций '
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
      @ 1,1 SAY 'Введите :'
      @ 3,1 SAY 'Hомер платежного поручения ' GET NPP
      @ 5,1 SAY 'Число                      ' GET DATA;
                 valid (val(data)>0.and.val(data)<=31)
      @ 7,1 SAY 'Hаименование получателя    ' GET POL1
      @ 8,29 GET POL2
      @ 9,29 GET POL3
      @ 10,29 GET POL4
      @ 12,1 SAY 'Hазначение платежа ' GET NZP1
      @ 13,21 GET NZP2
      @ 14,21 GET NZP3
      @ 15,21 GET NZP4
      @ 16,21 GET NZP5
      @ 18,1  say 'Сумма к оплате...........';
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
      @ 24,10 SAY '.....Включите печатающее устройство и ждите.....'
      SET PRINT ON
      set cons off
      ? replicate('-',79)
      ? '                                -------------          ---------  '
      ? '          ПЛАТЕЖHОЕ ПОРУЧЕHИЕ N | '+NPP+' |          |0401002|'
      ? '                                -------------          ---------  '
      ? '              '+DATA+' '+' '+TRIM(SUBS(MES,NMES,8))+' '+STR(GOD,4)+'г.'
      ? '                                                 ДЕБЕТ     СУММА  '
      ? ' Плательщик    '+DP1+' ------------------------'
      ? '               '+DP2+' |'+pred_RS+'|            |'
      ? '               '+DP3+' |         |'+STR(IT/100,12,2)+'|'
      ? '                                  ------------|сч.N     |            |'
      ? ' Банк плательщика   '+SUBS(pred_OB,1,13)+' |  '+pred_KB+'  |---------|            |'
      ? ' '+SUBS(pred_OB,14,31)+space(44-LEN(pred_OB))+;
      '  |Kод        |         |            |'
      ? space(34)+'-------------         |            |'
      ? ' Получатель  '+POL1+'    KРЕДИТ  |            |'
      ? ' '+POL2+'  ------------------------'
      ? '               '+POL3+' |'+SORG->RS+'|            |'
      ? '               '+POL4+' |сч.N     |            |'
      ? '                                  ------------|---------|------------|'
      ? ' Банк получателя    '+SUBS(SORG->OB,1,13)+' |  '+SORG->KB+'  |         |            |'
      ? ' '+SUBS(SORG->OB,14,31)+space(45-LEN(SORG->OB))+;
      ' |Kод        |сч.N     |            |'
      ? space(34)+'------------------------------------'
      ? SPACE(55)+' |пеня за     |'
      
      DLS=47
      V_ITOG=INT(IT/100)
      KOP=(IT/100-V_ITOG)*100
      STOR ' ' TO TSUM,TSUM1,TSUM2
      
*      DO SCRIPT WITH V_ITOG,TSUM,TSUM1,TSUM2,DLS
      
      IF LEN(TSUM2)=1
         TSUM1=TSUM1+'KОП. '+substr(STR(KOP+100,3),2,2)
      ELSE
         TSUM2=TSUM2+'KОП. '+substr(STR(KOP+100,3),2,2)
      ENDIF
      
      ? ' '+TSUM1+space(55-LEN(TSUM1))+'|из    %     |'
      ? ' '+TSUM2+space(55-LEN(TSUM2))+'|Р.          |'
      ? SPACE(55)+' |сумма с пен.|'
      ? ' Сумма прописью  '+space(39)+'|            |'
      ? space(55)+' --------------'
      ? ' Дата получения товара,оказания услуг             19  г.|Вид опер 01 |'
      ? space(55)+' |Hазн.пл. '+SNP->NP+'|'
      ? ' Hазначение платежа,наименование товара,выполненных     |Срок        |'
      ? ' работ,оказанных услуг,NN и суммы товарных документов   |N гр.       |'
      ? ' '+NZP1+'     |банка       |'
      ? ' '+NZP2+'     --------------'
      ? ' '+NZP3
      ? ' '+NZP4+'  Проведено банком'
      ? ' '+NZP5+'             19  г'
      ? '   М.П.        Подписи клиента'+SPACE(23)+'Подписи банка '
      ? ' '
      ? ' '
      ? ' '
      ? ' '
      set print off
      set cons on
      OTV=' '
      do AskYesNo with otv,24,'Повторить? ',ecol
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
