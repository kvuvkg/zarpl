PARA H1,H2,H3,H4
DaNet = 1
T_T = 2  && ฎชเใฃซฅญจฅ ฏฎแซฅ ง ฏ๏โฎฉ
R_R = 2  && ข๋ขฎค ญ  ฏฅ็ โ์ ฏฎแซฅ ง ฏ๏โฎฉ
if h3#'Z'
 nn='                '
 CLEAR
 set cursor on
 set confirm on
 @ 5,5 SAY 'ขฅคจโฅ ญ งข ญจฅ ฌฅแ๏ๆ  จ ฃฎค '
 @ 7,5 get nn picture 'xxxxxxxxxxxxx'
 @ 8,34 say " 1 -   / 2 - ฅโ "
 @ 8,5 say "ฅเฅๅฎค ญ  ญฎข๋ฉ ฌฅแ๏ๆ ก๋ซ?" get DaNet pict "9" range 1,2
 read
 set confirm off
 h4=nn
endif
Page_Nom=0
KSTR=int(Page_Size/2)*2  && ชฎซ-ขฎ แโเฎช ญ  ซจแโฅ (็ฅโญฎฅ)
R_ZSTR=6                 && ชฎซ-ขฎ แโเฎช ง ชซ๎็.็ แโจ (็ฅโญฎฅ)
SELE BASEF
LOCA ALL FOR STUD='2'
IAZ=IF(EOF(),1,2)
IJZ=1
DO WHILE IJZ <= IAZ
 R_NFIO=0
 V_ITOG=0     && ขแฅฃฎ
 R_LIST=0
 R_NL=0       && ฏเจงญ ช ญ ็ ซ  ซจแโ 
 STOR ' ' TO TSUM,TSUM1,TSUM2
 R_ROW=0
 IF H3='Z'
  R_H3='H  '
 ELSE
  R_H3='H  H'
 ENDI
 if tp_out=2
    clear
    @ 0,0 say ''
 else
    @ 5,8 SAY '  H  '+R_H3
    @ 6,2 SAY 'เฅคฏเจ๏โจฅ: '+ H1 + '  /  ฎคเ งคฅซฅญจฅ: ' + H2
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
     **ก๋ซฎ INT(.../100) กฅง ข๋็จโ ญจ๏ oz
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
   
   *** ฏฅ็ โ์ ง ฃฎซฎขช   ***
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
   ? '          H  N ____  ' + R_H3
   ? ''
   ? '           ' + H4 + ' '
   DO CASE
    CASE R_LIST < 9
     R_L=STR(R_LIST,1)
    CASE R_LIST >9 .AND. R_LIST < 99
     R_L=STR(R_LIST,2)
    CASE R_LIST > 99
     R_L=STR(R_LIST,3)
   ENDC
   ? '                                                                   จแโ ' + R_L
   ? replicate('-',79)
   ? '!   N   ! H!             !      !      ! - !'
   ? '!  /  ! H    !   H          !           ! B H ! H      !'
   ? '!       !          !                     !           !             !          !'
   ? replicate('-',79)
   R_ROW=16
  ENDI         &&คซ๏ R_NL=0
  *** ฏฅ็ โ์ แโเฎชจ คฎช-โ  ***
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
  * ฅ็ โ์ จโฎฃฎขฎฉ ฏซ โฅฆญฎฉ ขฅคฎฌฎแโจ *
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
  ? '  K    K'
  ? ''
  ? '  ________  ________ 19__ '
  ? ''
  ? ''
  *

V_TOGG=V_ITOG
  v_rub=int(v_itog)
  v_kop=(v_itog-v_rub)*100
 ? ' :'
    do prop with V_TOGG
  ? '                             ( . '+ltrim(STR(v_rub))+' K. '+;
                                    substr(str(v_kop+100,3),2,2)+')'
  ? ''
  ? ''
  ? ''
  ? '                               ______________________ K'
  ? ''
  ? ''
  ? '                               ______________________ .'
  ? ' "__"________19__'
  ? ''
  ? ''
  ? ''
  ? ''
  ? ''
  ? '          H  N____  '+R_H3
  ? ''
  ? '          ' + H4  + ' '
  ? ''
  ? ''
  ? ''
  ? ''
  ? '                                        K  '+R_L
  ? ''
  ? ''
  ? ''
  ? ''
  ? ' H H  H  __________'
  ? ''
  ? '____________________________________________________________'
  ? ''
  ? '____________________________(__________ .____K.)'
  ? ''
  ? ' HHO ______________________________________________'
  ? ''
  ? '____________________________(__________ .____K.)'
  ? ''
  ? ''
  ? '   _____________________________'
  ? ''
  ? '   ___________________________'
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
? '                                ' + ltrim(STR(L_ITOG,15,R_R))
V_ITOG=V_ITOG+L_ITOG
? ''
? ''
? ''
? ''
IF usl
 ? replicate('-',79)
ENDI
return
