parameters NextKod
SELE 0
USE SVO index svoindex
i=recc()
declare kodna[i], maskna[i]
do ReadMskM with kodna, maskna
sele 0
use suder index suderind
SELE 0
use skn
SET COLOR TO &STDC
CLEAR
@ 1,0
@ 1,18 SAY 'Лицевой счет за '+mesname+' '+Tek_God
sele a
first_lst=.t.
first_ras=.t.
RRR=1
itog_l=20
kop_row=19
DO WHILE RRR<>0
   sele basef
   if first_lst
     sele skn
     LOCA for KN=basef->NP
     sele basef
     @ 2,0
     @ 2,18 SAY  NAMEPOD
     @ 2,41 SAY  tn+' '+super->FIO
     SET COLOR TO &STDC
     @ 3,0 CLEAR
     @ 3,0,6,79 box frame+' '
     @ 3,13 say 'H а ч и c л е н и я'
     @ 3,53 say 'У д е р ж а н и я'
     @ 4,1  say 'Код  Код     Наименование    Дн.    Сумма   │Koд Код   Наименование    Сумма'
     @ 5,1  say 'меc. нач     начиcления      чаc            │меc уд.    удержания'
     end_shap=6
   endif
   setcolor(ecol)
   @ 23,0 clear
   @ 24,5 say '.....Ждите.....'
   setcolor(stdc)
   if first_lst
     sr=end_shap
     for N=1 to 12
       sr=sr+1
       nu=ltrim(str(n,2))
       do SayNaUd with .t.,sr,nu
       do SayNaUd with .f.,sr,nu
     next
   endif
   @ 7,45,18,45 box replicate('│',9)
   setcolor(mcolbln)
   @  7, 5 prompt '*'
   @  7,48 prompt '*'
   @  8, 5 prompt '*'
   @  8,48 prompt '*'
   @  9, 5 prompt '*'
   @  9,48 prompt '*'
   @ 10, 5 prompt '*'
   @ 10,48 prompt '*'
   @ 11, 5 prompt '*'
   @ 11,48 prompt '*'
   @ 12, 5 prompt '*'
   @ 12,48 prompt '*'
   @ 13, 5 prompt '*'
   @ 13,48 prompt '*'
   @ 14, 5 prompt '*'
   @ 14,48 prompt '*'
   @ 15, 5 prompt '*'
   @ 15,48 prompt '*'
   @ 16, 5 prompt '*'
   @ 16,48 prompt '*'
   @ 17, 5 prompt '*'
   @ 17,48 prompt '*'
   @ 18, 5 prompt '*'
   @ 18,48 prompt '*'
   @ kop_row,5  prompt '*'
   setcolor(stdc)
   @ kop_row,11 say 'Копейки прошл.меcяца'
   @ kop_row,36 say str(kpm,2)
   setcolor(stdc)
   do itogNU with itog_l,.f.
   first_lst=.f.
   SET COLOR TO &ECOL
   @ 23,0 clear
   @ 23,0 SAY '┘-ввод F1-помощь {Esc}-выход {9}-предыдущий {3}-след.работник F8-раcчет удерж.'
   @ 24,1 say 'F3-постоянная  F4-долговременная  F5-расчетная информация  F6-Архив'
   setcolor(mcolbln)
   PressKey=0
   set key 27 to
   set key 5  to down_shi
   set key 24 to up_shi
   set key 18 to PressCr
   set key  3 to PressCr
   set key -2 to PressCr
   set key -3 to PressCr
   set key -4 to PressCr
   set key -5 to PressCr
   set key -7 to PressCr
   set key 28 to PressCr
   set escape on
   menu to rrr
   set curs off
   set key 5  to
   set key 24 to
   set key 18 to
   set key  3 to
   set key -2 to
   set key -3 to
   set key -4 to
   set key -5 to
   set key -7 to
   set key 28 to
   keyboard ''
   setcolor(stdc)
   @ 23,0 clear
   do case
     case RRR=0
       NextKod=0
       exit
     case PressKey=-2 .or. PressKey=-3 .or. PressKey=-4 .or. PressKey=-5
       NextKod=PressKey
       exit
     case PressKey=18 .or. PressKey=3
       setcolor(stdc)
       do itogNU with itog_l,.t.,.f.
       do DnUpSotr
       loop
     case PressKey=-7
       do ReCalc
       loop
     case rrr=25
        @ kop_row,36 get kpm picture '##'
        read
        loop
   endcase
   rrr2=rrr
   vnu=int(rrr2/2)
   sr=vnu+end_shap
   if vnu*2=rrr2
      nach=.f.
   else
      nach=.t.
      vnu=vnu+1
      sr=sr+1
   endif
   nu=ltrim(str(vnu,2))
   sele basef
   set colo to &ecol
   @ 24,0
   @ 24,3 say 'Вводите код или нажмите: F1-помощь, {Esc}-отказ'
   SET COLOR TO &stdc
   set key 28 to HelpKod
   r_h=sr
   txt_h='naim'
   if nach
      spr_obl='svo'
      cr=6
      c_h=11
      fldn='vo'-nu
   else
      spr_obl='suder'
      cr=49
      c_h=54
      fldn='ku'-nu
   endif
   svo_upd=.f.
   suder_upd=.f.
   sele basef
   if PressKey=28
      keyboard chr(28)
   endif
   set curs on
   @ sr,cr GET &fldn;
           valid(tstkdolg('basef',spr_obl,fldn,'kvoy',r_h,c_h,txt_h,'s'))
   READ
   set curs off
   set key 28 to
   @ 24,0
   if svo_upd
      sele svo
      i=recc()
      if i<>len(kodna)
         rele kodna, maskna
         declare kodna[i], maskna[i]
      endif
      do ReadMskM with kodna, maskna
   endif
   sele basef
   setcolor(stdc)
   @ sr,cr say &fldn
   if lastkey()=27
      loop
   endif
   IF &fldn=' '
     if nach
        @ sr,2 clear to sr,44
        repl kmn&nu with ' ', vo&nu with ' ', dn&nu with 0, svon&nu with 0
     else
        @ sr,45 clear to sr,79
        repl kmu&nu with ' ', ku&nu with ' ', sku&nu with 0
     endif
     LOOP
   ENDIF
   SET COLOR TO &ECOL
   @ 23,0 clear
   @ 23,3 SAY 'Вводите данные: '
   sele basef
   if nach
      @ 23,row() say '1) код меcяца; 2) кол.дней или часы.минуты;'
      @ 24,1 say '3) cумму начиcленного или 0-для автоматич.расчета '+;
                 'или процент (например: 30% ) ' &&со знаком минус'
      setcolor(stdc)
      oldval=svon&nu/100
      isu=str(oldval,10,2)
      idni=dn&nu/100
       plus=if((val(&fldn)>=201.and.val(&fldn)<=210),5,0)
      set curs on
      @ sr,2 get kmn&nu   picture '99';
                          valid (val(kmn&nu)>0 .and. val(kmn&nu)<13;
                                 .and. .not.(' ' $ kmn&nu))
      @ sr,29 get idni    picture '###.##'  && valid ( testdni() )
      @ sr,35 get isu valid val(if('%' $ isu,left(isu,at('%',isu)-1),isu))<1000000000
      read
      set curs off
      if lastkey()<>27
         repl svon&nu with val(if('%' $ isu,left(isu,at('%',isu)-1),isu))*100,;
              dn&nu   with idni*100
         if ((val(isu)=0 .or. ('%' $ isu)) .and. pts $ '123')
            do NachInt with val(nu)
         endif
      endif
      do SayNaUd with nach,sr,nu
   else
      @ 23,row() say '1) код меcяца; 2) cумму удержаний'
      setcolor(stdc)
      isu=sku&nu/100
      set curs on
      @ sr,46 get kmu&nu   picture '99';
                           valid (val(kmu&nu)>0 .and. val(kmu&nu)<13;
                                 .and. .not.(' ' $ kmu&nu))
      @ sr,69 get isu      picture '########.##'
      read
      set curs off
      if lastkey()<>27
         repl sku&nu with isu*100
      endif
      do SayNaUd with nach,sr,nu
   endif
   sele basef
   rrr=rrr+2
ENDDO
setcolor(stdc)
do itogNU with itog_l,.t.,.f.
sele suder
use
sele skn
use
sele svo
use
sele basef
CLEAR
RETURN
*
proc Down_Shi
para p1,p2,p3
keyboard chr(19)+chr(19)
return
*
proc Up_Shi
para p1,p2,p3
keyboard chr(4)+chr(4)
return
*
proc NachInt
PARA II
SET TALK OFF
* MESPOD  C(2)  Расчетный месяц
* TEK_GOD C(4)  Расчетный год	
* DN_ARR[i]     Массив из 12-ти элементов,каждый N(2)
* II      N     Hомер группы в оперативной информации
PRIV I,J,II,A,K,GOD,GDR,GNS,B,wrk_dn,wrk_vo,wrk_val_vo,r, tk
I=ltrim(str(ii,2))   && IIF(II<10,STR(II,1),STR(II,2))
tk=TestKale()  && if(type('basef->kalendar')<>'C','',trim(kalendar))
DO CASE
 CASE PTS='1'
  A=DN_ARR&tk[VAL(KMN&I)]
 CASE PTS='2'
  a=100
 case PTS='3'
*  A=1
a=chas_arr&tk[val(kmn&i)]
 othe
  return
ENDC
GOD=VAL(TEK_GOD)
wrk_dn=dn&i/100
wrk_vo=vo&i
wrk_val_vo=val(wrk_vo)
IF wrk_val_vo=341 .and. pts='1'
a=chas_arr&tk[val(kmn&i)]  &&РАСЧЕТ 341 ПО ЧАСАМ У ВСЕХ
ENDIF
DO CASE
 CASE VAL(MESPOD)>VAL(KMN&I) .and. VAL(MESPOD)-VAL(KMN&I)>8
   GOD=VAL(TEK_GOD)+1
 CASE VAL(MESPOD)<VAL(KMN&I) .and. VAL(KMN&I)-VAL(MESPOD)>3
   GOD=VAL(TEK_GOD)-1
ENDC
IF (VAL(SUBS(TEK_GOD,3))-VAL(SUBS(datr,5))<0).OR.(VAL(SUBS(TEK_GOD,3))-VAL(SUBS(datr,5))=0.AND.VAL(SUBS(datr,5))=0)
 GDR=2000+VAL(SUBS(datr,5))
ELSE
 GDR=1900+VAL(SUBS(datr,5))
ENDI
IF (VAL(SUBS(TEK_GOD,3))-VAL(SUBS(NS,1,2))<0).OR.(VAL(SUBS(TEK_GOD,3))-VAL(SUBS(ns,1,2))=0.AND.VAL(SUBS(ns,1,2))=0)
 GNS=2000+VAL(SUBS(ns,1,2))
ELSE
 GNS=1900+VAL(SUBS(ns,1,2))
ENDI
k=1
DO CASE
* Отпускные
 CASE wrk_val_vo>=201.and.wrk_val_vo<=205
  COD=OZM/25.200
  DO CODBOLO WITH COD
  REPL SVON&I WITH TestSvon(COD*wrk_dn)
  RETU
 CASE wrk_val_vo>=206.and.wrk_val_vo<=210
  COD=OHM/25.200
  DO CODBOLO WITH COD
  REPL SVON&I WITH TestSvon(COD*wrk_dn)
  RETU
 case wrk_val_vo>=241.and.wrk_val_vo<=250
  J=ltrim(str(ii-1))   &&IIF(II-1<10,STR(II-1,1),STR(II-1,2))
  cod=IF(VAL(I)<=7.AND.VO&I=NVU&J .and. svon&i=0,NSU&J,svon&i)
  kmi=substr(str(val(kmn&i)+100,3),2,2)
  COD=cods&kmi*cod/100                         &&&&&
  DO CODBOLO WITH COD
  REPL SVON&I WITH TestSvon(COD*wrk_dn)
  RETU
 CASE (wrk_val_vo>=221.and.wrk_val_vo<=240) .or.;
     ((wrk_val_vo>=211.and.wrk_val_vo<=215) .and. stud='5')
  kmi=substr(str(val(kmn&i)+100,3),2,2)
  cod=if(wrk_val_vo>=211.and.wrk_val_vo<=215,(OZM+OHM)/25.400,cods&kmi)
  DO CODBOLO WITH COD
  REPL SVON&I WITH TestSvon(COD*wrk_dn)
  RETU
 CASE wrk_val_vo>=211.and.wrk_val_vo<=215
  B=0
  DO CASE
   CASE STUD $ '23'
    B=8000
   CASE STUD='4'
    B=10000
  ENDC
  COD=MIN(B/DN_ARR&tk[VAL(KMN&I)],(OZM+OHM)/25.400)  &&MIN(B,(OZM+OHM))/DN_ARR&tk[VAL(KMN&I)]
  DO CODBOLO WITH COD
  REPL SVON&I WITH TestSvon(COD*wrk_dn)
  RETU
* Больничные
 CASE wrk_val_vo>=216.and.wrk_val_vo<=220
  pos=0
  DO CASE
   CASE GOD-GNS<3
    POS=50
    IF GOD-GDR<18.OR.GOD-GDR=18.AND.VAL(KMN&I)<VAL(SUBS(datr,3,2))
     POS=60
    ENDI
   CASE GOD-GNS=3
    DO CASE
     CASE VAL(KMN&I)<VAL(SUBS(ns,3,2))
      POS=50
     OTHE
      POS=60
    ENDC
   CASE GOD-GNS<5
    POS=60
   CASE GOD-GNS=5
    DO CASE
     CASE VAL(KMN&I)<VAL(SUBS(ns,3,2))
      POS=60
     OTHE
      POS=80
    ENDC
   CASE GOD-GNS<8
    POS=80
   CASE GOD-GNS=8
    DO CASE
     CASE VAL(KMN&I)<VAL(SUBS(ns,3,2))
      POS=80
     OTHE
      POS=100
    ENDC
   othe
    pos=100
  ENDC
  kmi=substr(str(val(kmn&i)+100,3),2,2)
  COD=codb&kmi
  povb=100
  n_mes=val(kmn&i)
  DO CODBOLO WITH COD,POS,POVB,n_mes
  REPL SVON&I WITH TestSvon((COD*wrk_dn*POS*POVB)/10000)
  RETU
 CASE wrk_val_vo>=346.and.wrk_val_vo<=350
  K=2
 CASE wrk_val_vo>=351.and.wrk_val_vo<=355
  K=0.5*if(pts='2',prod_rd&tk,1)
 CASE wrk_val_vo=356
  K=0
  DO CASE
   CASE PTS='2'
    K=1
  ENDC
ENDC
DO CASE
 CASE (wrk_val_vo>=101.AND.wrk_val_vo<=103).OR.wrk_val_vo=109;
  .OR.(wrk_val_vo>=341.AND.wrk_val_vo<=356);
  .or.((wrk_val_vo>=211.and.wrk_val_vo<=215) .and. stud='1')
  REPL SVON&I WITH TestSvon(round((OKS*wrk_dn/A)*K,0))
 CASE wrk_val_vo>=331.and.wrk_val_vo<=340
  J=ltrim(str(ii-1))   &&IIF(II-1<10,STR(II-1,1),STR(II-1,2))
  IF VAL(I)<=7.AND.VO&I=NVU&J
     REPL SVON&I WITH NSU&J
  ENDI
  REPL SVON&I WITH TestSvon(round(SVON&I*wrk_dn/A,0))
 CASE wrk_val_vo>=301.and.wrk_val_vo<=320.or.wrk_val_vo=395
  J=ltrim(str(ii-1))   &&IIF(II-1<10,STR(II-1,1),STR(II-1,2))
  IF VAL(I)<=7.AND.VO&I=NVU&J .and. svon&i=0
   REPL SVON&I WITH NSU&J
  ENDI
  Oklad = OKS
  if mPen > 0
    Oklad = mPen
  Endif
  REPL SVON&I WITH TestSvon(round((Oklad*wrk_dn*SVON&I/A)/10000,0))
 CASE wrk_val_vo>=321.and.wrk_val_vo<=330
  J=ltrim(str(ii-1))   &&IIF(II-1<10,STR(II-1,1),STR(II-1,2))
  IF VAL(I)<=7.AND.VO&I=NVU&J .and. svon&i=0
     REPL SVON&I WITH NSU&J
  ENDI
if wrk_val_vo=322
*  if pts='2'
*  repl svon&i with TestSvon(round( OKS/100*wrk_dn*0.04,-3))
*  else
*  repl svon&i with TestSvon(round( OKS/chaS_arr[val(kmn&i)]*wrk_dn*0.04,-3))
*  end
 repl svon&i with;
  TestSvon(round((oks*wrk_dn*svon&i/if(pts='2',a,chas_arr[val(kmn&i)]))/10000,0))
else
 Oklad = Oks
 if mPen > 0
   Oklad = mPen
 Endif
 repl svon&i with testsvon(round((Oklad*wrk_dn*svon&i/if(pts='2',a,a*prod_rd&tk))/10000,0))
endif
 ENDC
 if wrk_val_vo=366 .or.  wrk_val_vo=362 .or.  wrk_val_vo=361
    do SeveRaio
 endif
RETU
*
proc CODBOLO
para p1,p2,p3,p4
priv pp1,pp2,pp3,i,procents,scr,a
if pcount()>2
   pp2=p2
   pp3=p3
   i_scr=20
   procents=.t.
else
   stor 0 to pp2,pp3
   i_scr=16
   procents=.f.
endif
pp1=p1
scr=savescreen(10,10,i_scr,75)
setcolor(bcol)
@ 10,10,i_scr,75 box framedbl+'░'
setcolor(ecol)
@ 12,12 say '...Ждите...'
setcolor(stdc)
if procents
   if pts $ '123'
      do case
         case pts='1'
            aa=1/dn_arr&tk[p4]
         case pts='2'
            aa=prod_rd&tk
         case pts='3'
            aa=1
      endcase
      r=2*oks*aa
      if pp1>=r              && 2*oks*aa
         stor 0 to ss,sb
         do SSSB with ss,sb
         r=2*(oks+sb)*aa
      endif
      if pp1>=r
         @ 14,12 say ' Двойная ставка                          '+;
                     str(r/100,9,2)
      endif
   endif
   pp1=p1/100
   @ 16,12 say ' Цена одного дня  (руб.коп.)                ';
           get pp1 pict '#####.##'
   @ 17,12 say ' % оплаты больничных (от стажа)               ';
           get pp2 pict '###'
   @ 18,12 say ' % оплаты в зависимости от вида больничного   ';
           get pp3 pict '###'
else
   pp1=p1/100
   @ 14,12 say ' Цена одного дня  (руб.коп.)                  ';
           get pp1 pict '#####.###'
endif
setcolor(ecol)
@ 12,12 say ' Измените или нажмите Esc: '
setcolor(stdc)
set curs on
read
set curs off
if lastkey()<>27
   p1=pp1*100
   p2=pp2
   p3=pp3
endif
restscreen(10,10,i_scr,75,scr)
return
