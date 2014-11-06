PARA H1,H2,H3,H4
DaNet = 1
T_T = 2  && округление после запятой
R_R = 2  && вывод на печать после запятой
if h3#'Z'
 nn='                '
 CLEAR
 set cursor on
 set confirm on
 @ 5,5 SAY 'Введите название месяца и года'
 @ 7,5 get nn picture 'xxxxxxxxxxxxx'
 @ 8,34 say " 1 - Да / 2 - Нет "
 @ 8,5 say "Переход на новый месяц был?" get DaNet pict "9" range 1,2
 read
 set confirm off
 h4=nn
endif
Page_Nom=0
KSTR=int(Page_Size/2)*2  && кол-во строк на листе (четное)
R_ZSTR=6                 && кол-во строк заключ.части (четное)
SELE BASEF
LOCA ALL FOR STUD='2'
IAZ=IF(EOF(),1,2)
IJZ=1
DO WHILE IJZ <= IAZ
 R_NFIO=0
 V_ITOG=0     && всего
 R_LIST=0
 R_NL=0       && признак начала листа
 STOR ' ' TO TSUM,TSUM1,TSUM2
 R_ROW=0
 IF H3='Z'
  R_H3='HА ВЫДАЧУ ЗАРПЛАТЫ'
 ELSE
  R_H3='HА ВЫДАЧУ АВАHСА'
 ENDI
 if tp_out=2
    clear
    @ 0,0 say ''
 else
    @ 5,8 SAY 'ИДЕТ ПЕЧАТЬ ПЛАТЕЖHОЙ ВЕДОМОСТИ '+R_H3
    @ 6,2 SAY 'Предприятие: '+ H1 + '  /  Подразделение: ' + H2
 endif
 * *
 IF IJZ=1
  filt="stud<>'2'"
 else
  filt="stud='2'"
 endif
 sele super
 set filter to rpv $ '12' &&.and. ys $ '01'
 seek kodpod &&go top
 DO WHIL .NOT.EOF() .and. ksp=kodpod
  sele super
  S_FIO=0
  DO WHIL S_FIO <=0 .AND. .NOT. EOF() .and. ksp=kodpod
   sele basef
   go super->base_rec
   if .not. (ys $ '01') .or. .not. &filt
      sele super
      skip
      loop
   endif
   DO CASE
    CASE H3='Z' .AND. YS $ '01'
     S_FIO=((KPM+svon1+svon2+svon3+svon4+svon5+svon6+svon7;
     +svon8+svon9+svon10+svon11+svon12)-(SKU1+SKU2+SKU3+SKU4+SKU5;
     +SKU6+SKU7+SKU8+SKU9+SKU10+SKU11+SKU12) - oz)/100
     **было INT(.../100) без вычитания oz
    CASE H3='A' .AND. YS $ '01' .AND. RPV='1' .AND. STUD<>'2'
     if DaNet = 1
       j=AvanSeek('915')
       if j<>0
          j=ltrim(str(j,2))
          S_FIO=SKU&j/100     
       endif
     else
          S_FIO = PA/100
     Endif
   ENDC
   sele super
   IF S_FIO <= 0
    SKIP
   ENDI
  ENDD
  IF EOF() .or. ksp<>kodpod
   EXIT
  ENDI
  IF R_ROW + R_ZSTR = KSTR
   R_NL=0
   do ItogPoLi with (R_NL=0 .AND. R_PECH=0)
  ENDI
  IF R_NL=0
   R_LIST=R_LIST + 1
   R_NL=1
   L_ITOG=0
   
   if Page_Nom=0
      Page_Nom=1
   else
      do EndPage
   endif
   
   *** печать заголовка  ***
   IF R_PECH=0
    ? replicate('-',79)
   ENDI
   if r_pech=0
      ? ''
      ? ''
      ? ''
   endif
   ? H1
   ? ''
   ? H2
   ? ''
   ? '          ПЛАТЕЖHАЯ ВЕДОМОСТЬ N ____  ' + R_H3
   ? ''
   ? '          ЗА ' + H4 + ' ГОДА'
   DO CASE
    CASE R_LIST < 9
     R_L=STR(R_LIST,1)
    CASE R_LIST >9 .AND. R_LIST < 99
     R_L=STR(R_LIST,2)
    CASE R_LIST > 99
     R_L=STR(R_LIST,3)
   ENDC
   ? '                                                                   Лист ' + R_L
   ? replicate('-',79)
   ? '!   N   ! ТАБЕЛЬHЫЙ!   ФАМИЛИЯ  И        ! С У М М А !  РОСПИСЬ    ! ПРИМЕЧА- !'
   ? '!  П/П  ! HОМЕР    !   ИHИЦИАЛЫ          !           ! B ПОЛУЧЕHИИ ! HИЕ      !'
   ? '!       !          !                     !           !             !          !'
   ? replicate('-',79)
   R_ROW=16
  ENDI         &&для R_NL=0
  *** печать строки док-та ***
  ? ''
  R_NFIO=R_NFIO+1
  DO CASE
   CASE R_NFIO < 10
    S_NFIO='  '+STR(R_NFIO,1)
   CASE R_NFIO > 9 .AND. R_NFIO < 100
    S_NFIO=' '+STR(R_NFIO,2)
   CASE R_NFIO > 99
    S_NFIO=STR(R_NFIO,3)
  ENDC
  if row()>Page_Size .and. tp_out=2
    do EndPage
  endif

  S_fio = ROUND(S_FIO,T_T)
  ? '   '+S_NFIO+'    '+TN+'    '+SUPER->FIO+' '+STR(S_FIO,12,R_R)+;
    '  ___________'
  L_ITOG=L_ITOG+S_FIO
  R_ROW=R_ROW+2
  sele super
  SKIP
 ENDD
 * *
 if tp_out=2
    do EndPage
 endif
 IF R_ROW + R_ZSTR < KSTR
  DO WHILE R_ROW + R_ZSTR < KSTR
   ? ''
   ? ''
   R_ROW=R_ROW+2
  ENDD
 ENDI
 IF R_NL=1
  do ItogPoLi with (R_PECH=0)
 ENDI
 IF V_ITOG<>0
  * Печать итоговой платежной ведомости *
  do EndPage
  IF R_PECH=0
    ? replicate('-',79)
  ENDI
  ? ''
  ? H1
  ? ''
  ? H2
  ? ''
  ? ''
  ? ' В KАССУ ДЛЯ ОПЛАТЫ В СРОK'
  ? ''
  ? ' С ________ ПО ________ 19__ Г'
  ? ''
  ? ''
  *

V_TOGG=V_ITOG
  v_rub=int(v_itog)
  v_kop=(v_itog-v_rub)*100
 ? 'В СУММЕ:'
    do prop with V_TOGG
  ? '                             ( ГРВ. '+ltrim(STR(v_rub))+' KОП. '+;
                                    substr(str(v_kop+100,3),2,2)+')'
  ? ''
  ? ''
  ? ''
  ? '                               ______________________ РУKОВОДИТЕЛЬ'
  ? ''
  ? ''
  ? '                               ______________________ ГЛ.БУХГАЛТЕР'
  ? ' "__"________19__Г'
  ? ''
  ? ''
  ? ''
  ? ''
  ? ''
  ? '          ПЛАТЕЖHАЯ ВЕДОМОСТЬ N____  '+R_H3
  ? ''
  ? '         ЗА ' + H4  + ' ГОДА'
  ? ''
  ? ''
  ? ''
  ? ''
  ? '                                        KОЛИЧЕСТВО ЛИСТОВ '+R_L
  ? ''
  ? ''
  ? ''
  ? ''
  ? 'ПО HАСТОЯЩЕЙ ПЛАТЕЖHОЙ ВЕДОМОСТИ ВЫПЛАЧЕHО РУБЛЕЙ __________'
  ? ''
  ? '____________________________________________________________'
  ? ''
  ? '____________________________(__________ ГРВ.____KОП.)'
  ? ''
  ? 'И ДЕПОHИРОВАHO ______________________________________________'
  ? ''
  ? '____________________________(__________ ГРВ.____KОП.)'
  ? ''
  ? ''
  ? ' ВЫПЛАТУ ПРОИЗВЕЛ _____________________________'
  ? ''
  ? ' ПРОВЕРИЛ БУХГАЛТЕР ___________________________'
  ? ''
  if r_pech=0
     ? ''
     ? ''
     ? ''
  endif
  IF R_PECH=0
   ? replicate('-',79)
   ? ''
   *eject
  ENDI
 ENDI
 IJZ=IJZ+1
ENDD
sele basef
set filter to
CLEA
RETU
*
proc ItogPoLi
para usl
? ''
? '      И Т О Г О  П О  Л И С Т У             ' + ltrim(STR(L_ITOG,15,R_R))
V_ITOG=V_ITOG+L_ITOG
? ''
? ''
? ''
? ''
IF usl
 ? replicate('-',79)
ENDI
return
