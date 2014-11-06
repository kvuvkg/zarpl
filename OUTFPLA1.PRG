PARA H1,H2,H3,H4
CLEAR
Page_Nom=0
KSTR=int(Page_Size/2)*2  && ª®«-Ά® αβΰ®ª ­  «¨αβ¥ (η¥β­®¥)
R_ZSTR=6                 && ª®«-Ά® αβΰ®ª § ª«ξη.η αβ¨ (η¥β­®¥)
SELE BASEF
LOCA ALL FOR STUD='2'
IAZ=IF(EOF(),1,2)
IJZ=1
DO WHILE IJZ <= IAZ
 R_NFIO=0
 V_ITOG=0     && Άα¥£®
 R_LIST=0
 R_NL=0       && ―ΰ¨§­ ª ­ η «  «¨αβ 
 STOR ' ' TO TSUM,TSUM1,TSUM2
 R_ROW=0
IF H3='D'
  R_H3=' „……’€     '
ENDIF
 if tp_out=2
    clear
    @ 0,0 say ''
 else
    @ 5,8 SAY '„…’ …—€’ ‹€’…†H‰ ‚…„‘’ '+R_H3
    @ 6,2 SAY 'ΰ¥¤―ΰ¨οβ¨¥: '+ H1 + '  /  ®¤ΰ §¤¥«¥­¨¥: ' + H2
 endif
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
    CASE H3='D' .AND. YS $ '01'
IF ZND#0
     S_FIO=ZND/100
ENDIF
    CASE H3='D' .AND. YS $ '01' .AND. RPV='1' .AND. STUD<>'2'
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
   do ItogPoLi1 with (R_NL=0 .AND. R_PECH=0)
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
   *** ―¥η βμ § £®«®Άª   ***
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
   ? '          ‹€’…†H€ ‚…„‘’ N ____  ' + R_H3
   ? ''
   ? '          ‡€ ' + H4 + ' ƒ„€'
   DO CASE
    CASE R_LIST < 9
     R_L=STR(R_LIST,1)
    CASE R_LIST >9 .AND. R_LIST < 99
     R_L=STR(R_LIST,2)
    CASE R_LIST > 99
     R_L=STR(R_LIST,3)
   ENDC
   ? '                                                                   ‹¨αβ ' + R_L
   ? replicate('-',79)
   ? '!   N   ! ’€…‹H›‰!   ”€‹          ! ‘ “   € !  ‘‘    ! …—€- !'
   ? '!  /  ! H…    !   H–€‹›          !           ! B ‹“—…H ! H…      !'
   ? '!       !          !                     !  B “.   !             !          !'
   ? replicate('-',79)
   R_ROW=16
  ENDI         &&¤«ο R_NL=0
  *** ―¥η βμ αβΰ®ª¨ ¤®ª-β  ***
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
IF S_FIO#0
  ? '   '+S_NFIO+'     '+TN+'   '+SUPER->FIO+'   '+STR(S_FIO,9,2)+;
    '  ___________'
ENDIF

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
  do ItogPoLi1 with (R_PECH=0)
 ENDI
 IF V_ITOG<>0
  * ¥η βμ ¨β®£®Ά®© ―« β¥¦­®© Ά¥¤®¬®αβ¨ *
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
  ? ' ‚ K€‘‘“ „‹ ‹€’› ‚ ‘K'
  ? ''
  ? ' ‘ ________  ________ 19__ ƒ'
  ? ''
  ? ''
  *

V_TOGG=V_ITOG
  v_rub=int(v_itog)
  v_kop=(v_itog-v_rub)*100
 ? '‚ ‘“…:'
    do prop with V_TOGG
  ? '                             ( . '+ltrim(STR(v_rub))+' K. '+;
                                    substr(str(v_kop+100,3),2,2)+')'
  ? ''
  ? ''
  ? ''
  ? '                               ______________________ “K‚„’…‹'
  ? ''
  ? ''
  ? '                               ______________________ ƒ‹.“•ƒ€‹’…'
  ? ' "__"________19__ƒ'
  ? ''
  ? ''
  ? ''
  ? ''
  ? ''
  ? '          ‹€’…†H€ ‚…„‘’ N____  '+R_H3
  ? ''
  ? '         ‡€ ' + H4  + ' ƒ„€'
  ? ''
  ? ''
  ? ''
  ? ''
  ? '                                        K‹—…‘’‚ ‹‘’‚ '+R_L
  ? ''
  ? ''
  ? ''
  ? ''
  ? ' H€‘’™…‰ ‹€’…†H‰ ‚…„‘’ ‚›‹€—…H “‹…‰ __________'
  ? ''
  ? '____________________________________________________________'
  ? ''
  ? '____________________________(__________ .____K.)'
  ? ''
  ? ' „…H‚€HO ______________________________________________'
  ? ''
  ? '____________________________(__________ .____K.)'
  ? ''
  ? ''
  ? ' ‚›‹€’“ ‡‚…‹ _____________________________'
  ? ''
  ? ' ‚…‹ “•ƒ€‹’… ___________________________'
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
proc ItogPoLi1
para usl
? ''
? '       ’  ƒ      ‹  ‘ ’ “               ' + ltrim(STR(L_ITOG))
V_ITOG=V_ITOG+L_ITOG
? ''
? ''
? ''
? ''
IF usl
 ? replicate('-',79)
ENDI
return
