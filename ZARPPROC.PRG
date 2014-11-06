procedure OpenABC
para f,zer
priv f1,i,o1,o2,o3,zer1,wrk,i_base
i_base=0
if pcount()>1
   zer1=zer+'000'
else
   zer1='abc'
endif
o1=substr(zer1,1,1)
o2=substr(zer1,2,1)
o3=substr(zer1,3,1)
f1=upper(f)
if 'B' $ f1
   sele &o2
   use superlist index superpono,supertn alias super
endif
if 'A' $ f1
   sele &o1
   use  &dm.:base&kodpod alias basef
   i_base=recc()*12
endif
if 'C' $ f1
   sele &o3
   if .not. file(dm-':seba'-kodpod-'.dbf')
      use seba
      copy stru to &dm.:seba&kodpod
   endif
   use &dm.:seba&kodpod alias sebaf
   sele sebaf
   do while recc()<i_base
      append blank
   enddo
endif
sele basef
return
*
Procedure AskYesNo
Parameters r,line,prom,ecol
priv oldcol,iask
oldcol=setcolor('n/')
@ line,69
setcolor(ecol)
@ line,0 clear to line,68
@ line,0 say left(prom+space(68),68)
setcolor(mcolbln1)
@ line,70 prompt ' Да '
@ line,75 prompt ' Нет '
iask=2
keyboard ""
menu to iask
r=if(iask=1,'D','N')
setcolor(oldcol)
return
*
proc NullKeyB
keyboard ''
return
*
Procedure AddKey
parameters pn,ln,vn
clear gets
return
*
Procedure PressCr
parameters pn,ln,vn
PressKey=lastkey()
clear gets
keyboard chr(13)
return
*
Procedure PressInt
clear gets
keyboard chr(27)
return
*
Procedure PressEsc
parameters pn,ln,vn
clear gets
return
*
Procedure DosExit
priv mycol
save screen to dos_s
mycol=setcolor(stdc)
@ 0,0 clear to 2,79
@ 0,3,2,56 box frame+' '
@ 1,5 say 'Выход в DOS. Для возврата введите EXIT и нажмите ┘'
sprcoms=gete('COMSPEC')
set curs on
! &sprcoms
rest screen from dos_s
setcolor(mycol)
release dos_s
keyboard ''
return
*
Procedure ListMenu
Parameters MenuRout,x1,y1,x2,y2,lines,recs,textp,line24,top_txt,bot_txt,xb,yb,;
           TopRout
Private i,have,PageKey,test_bad,www
test_bad=if(type('yb')="C",yb,'.f.')
xb=x1
yb=y2
have=.t.
save screen to MenuScr
do while .t.
 rest screen from MenuScr
 setcolor(mcol)
 if recs>0 .and. recs<=reccount()
  go recs
 else
  go top
  if eof() .or. &test_bad
   @ x1,y1 prompt '-----------Нет данных---------------'
   yb=max(col(),yb)
   xb=x1+1
   have=.f.
  else
   recs=recno()
  endif
 endif
 i=x1
 PageKey=0
 do while i<=x2 .and. .not. ( eof()  .or. &test_bad )
  setcolor(mcolbln)
  @ i,y1 prompt '*'
  setcolor(stdc)
  @ i,y1+1 say &textp 
  yb=max(col(),yb)
  i=i+1
  xb=i
  PageKey=inkey()
  if PageKey=3 .or. PageKey=18 .or. PageKey=27
     keyboard chr(PageKey)
     exit
  endif
  PageKey=0
  skip
 enddo
 if PageKey=0
    setcolor(bcol)
    do boxer with x1-1,y1-1,xb,yb,frame
    @ x1-1,y1 say top_txt
    @ i,y1 say bot_txt+;
               if((eof().or.&test_bad),'*конец списка*','*есть продолжение*')
 endif
 set key 18 to PressCr
 set key 3  to PressCr
 set key 27 to
 PressKey=0
 setcolor(ecol)
 www=int(len(line24)/80)
 iline=24-www+if(www*80=len(line24),1,0) &&iline=if((len(line24)>80 .and. xb<=22),23,24)
 @ iline,0 clear
 @ iline,0 say line24
 setcolor(mcol)
 if ''<>trim(menurout)
    do &menurout with MenuRout,x1,y1,x2,y2,lines,recs,textp,line24,top_txt,bot_txt,xb,yb
 endif
 setcolor(mcolbln)
 menu to lines
 if lines<>0
    setcolor(stdc)
    go recs
    skip lines-1
    @ x1+lines-1,y1 say '*'+&textp
 endif
 setcolor(stdc)
 @ iline,0 clear
 set key 18 to
 set key 3  to
 if PressKey<>0 .and. have
  go recs
  do case
   case PressKey=18
    skip x1-x2
    if bof() .or. &test_bad
     if type('TopRout')='C'
        do &TopRout
     else
        go top
     endif
    endif
   case PressKey=3
    skip x2-x1
    if eof() .or. &test_bad
     go recs
    endif
   othe
    exit
  endcase
  recs=recno()
 else
  exit
 endif
enddo
rele MenuScr
return
*
Procedure Boxer
parameters x1b,y1b,x2b,y2b,frameb
priv i,txt0,txt1
@ x1b,y1b say substr(frameb,1,1)
if y1b<y2b-1
   @ X1B,y1b+1 say replicate(substr(frameb,2,1),y2b-y1b)
   @ X2B,y1b+1 say replicate(substr(frameb,6,1),y2b-y1b)
endif
@ x1b,y2b say substr(frameb,3,1)
@ x2b,y2b say substr(frameb,5,1)
if x1b<x2b-1
   @ x1b+1,y1b,x2b-1,y1b box replicate(substr(frameb,4,1),9)
   @ x1b+1,y2b,x2b-1,y2b box replicate(substr(frameb,8,1),9)
endif
@ x2b,y1b say substr(frameb,7,1)
return
*
Proc HelpKod
PARA pn, pl, fldp
priv wrkn,wrktxt,xb,yb,ptxt,fld,m1,cur_sel,old_c_hel
set key 28 to
save screen to HelpScr
cur_sel=str(select())
fld=trim(upper(fldp))
old_c_hel=setcolor()
declare fieldH[2]
do case
   case fld=='TN'
      korrf='SUPER'
      wrktxt='Список работающих'
      fieldH[1]='tn'
      fieldH[2]='fio'
   case fld=='KALENDAR'
      korrf='SUPERSRD'
      wrktxt='Графики работы'
      fieldH[1]='kod'
      fieldH[2]='naim'
   case fld=='KPS'
      korrf='SPSZ'
      wrktxt='Произв.cтатьи затрат'
      fieldH[1]='kps'
      fieldH[2]='naim'
   case substr(fld,1,3)='NVU' .or. substr(fld,1,2)='VO'
      korrf='SVO'
      wrktxt='Виды оплат'
      fieldH[1]='kvoy'
      fieldH[2]='naim'
   case substr(fld,1,3)='UVU' .or. substr(fld,1,2)='KU'
      korrf='SUDER'
      wrktxt='Виды удержаний'
      fieldH[1]='kvoy'
      fieldH[2]='naim'
   case substr(fld,1,3)='UKO'
      korrf='SORG'
      wrktxt='Организации'
      fieldH[1]='koo'
      fieldH[2]='naim'
   case substr(fld,1,4)='UKPA'
      korrf='SADRA'
      wrktxt='Получатели алиментов'
      fieldH[1]='kap'
      fieldH[2]='fia'
   case substr(fld,1,3)='UOP'
      korrf='SKVO'
      wrktxt='Виды операций'
      fieldH[1]='kvo'
      fieldH[2]='naim'
   case fld=='KR' .or. fld=='KKP'
      korrf='SKR'
      wrktxt='Kатегории работающих'
      fieldH[1]='kkp'
      fieldH[2]='naim'
   case fld=='KP'
      korrf='SKPR'
      wrktxt='Профеccии'
      fieldH[1]='kp'
      fieldH[2]='naim'
   case fld=='NP'
      korrf='SKN'
      wrktxt='Hалогоплательщики'
      fieldH[1]='kn'
      fieldH[2]='naim'
   case fld=='VH'
      korrf='SNETR'
      wrktxt='Виды пособий по врем.нетруд-ти'
      fieldH[1]='kvn'
      fieldH[2]='naim'
   case fld=='VP'
      korrf='SKVU'
      wrktxt='Почтовый сбор'
      fieldH[1]='kyps'
      fieldH[2]='naim'
   case fld=='PB'
      korrf='SVOZR'
      wrktxt='Kатегории пенсионеров по возрасту'
      fieldH[1]='kkvp'
      fieldH[2]='naim'
   case fld=='VR'
      korrf='svr'
      wrktxt='Виды работ'
      fieldH[1]='kvr'
      fieldH[2]='naim'
   othe
      return
endcase
txtcorr=if(select(SprParm)<>0.or.fld='TN','','{0}/{Ins}/{ВСТ}-корр.cправ.')
sele &korrf
do while .t.
   sele &korrf
   go top
   setcolor(ecol)
   @ 24,0
   @ 24,1 say '{стрелки}-движение {Esc}/{Ключ}-конец ┘-выбор '+txtcorr
   setcolor(stdc)
   a=fieldH[1]
   b=fieldH[2]
   left_b=max(75-max(len(&a),len(trim(a)))-max(len(&b),len(trim(b))),0)
   @ 0,left_b,23,79 box replicate('░',9)
   setcolor(ucol)
   setcolor(stdc)
   if select(SprParm)=0 .and. fld<>'TN'
      set key 22 to PressCr
   endif
   PressKey=0
   Press1st=.t.
   shap=' '+left(wrktxt,min(76-left_b,len(wrktxt)))
   keyboard chr(5)
   setcolor(mcol)
   set key 19 to NullKeyB
   set key  4 to NullKeyB
   set curs off
   dbedit(1, left_b+1, 23, 78, fieldH, "helplist", .T., .T., .T., .T., "═╧═")
   set curs on
   set key 22 to
   set key 19 to
   set key  4 to
   setcolor(stdc)
   @ 24,0
   if PressKey=22
      sele 0
      use &SprParm
      loca for upper(file)=upper(korrf)
      if eof()
         txtcorr=''
         use
         loop
      endif
      SprNom=recno()
      wrk   = proc
      s_col = colshap
      Shap  = trim(shapka)
      txt1  = trim(txt)
      file1 = file
      maxf1 = maxfldn
      use
      sele &korrf
      use
      sele 0
      let_do=.f.
      do &wrk with txt1,file1,SprNom,maxf1
      sele 0
      do case
       case upper(korrf)=='SVO'
         use svo
         index on kvoy to svoindex &&temp
         svo_upd=.t.
       case upper(korrf)=='SUDER'
         use suder
         index on kvoy to suderind &&temp1
         suder_upd=.t.
       case upper(korrf)=='SUPERSRD'
         sele 0
         use supersrd
       other
         use &korrf
      endcase
   else
     exit
   endif
enddo
sele &cur_sel
if lastkey()=27
   keyboard ''
else
   wrkn=fieldH[1]
   repl &fld with &korrf.->&wrkn
   clear gets
endif
rest screen from HelpScr
rele HelpScr
setcolor(stdc)
sele &korrf
@ r_h,c_h say if(lastkey()=27,space(len(&txt_h)),&txt_h)
sele &cur_sel
setcolor(old_c_hel)
set key 28 to HelpKod
return
*
FUNCTION HelpList
PARAMETERS mode,i_cur
if Press1st
 @ 1,left_b+1 clear to 1,78
 @ 1,left_b+1 say shap
 Press1st=.f.
endif
RETURN(if(mode=4.and.(lastkey()=27.or.lastkey()=13),0,1))
FUNCTION print_error
PARAM name, line
PRIVATE key, file, oldc, ret_c, Scr
oldc=setcolor(ecol)
ret_c=.f.
Scr=savescreen(22,0,24,79)
SET DEVICE TO SCREEN
@ 22, 0 clear to 24,79
@ 23, 0 SAY " Включите печатающее устройство и (или) "
@ 24, 0 SAY " нажмите: <I>-игнороровать, <R>-повторить, <Q>-завершить работу..."
DO WHILE .T.
 key = UPPER(CHR(INKEY(0)))
 IF (M->key == "Q")
  QUIT
 ELSEIF (M->key == "I")
  ret_c=.F.
  exit
 ELSEIF (M->key == "R")
  ret_c=.T.
  exit
 END
END
setcolor(oldc)
restscreen(22,0,24,79,Scr)
RETURN ret_c
*
proc ReadMskM
para kodna,maskna
priv i,j
i=len(kodna)
j=1
go top
do while .not.eof() .and. j<=i
   kodna [j] = kvoy
   if pcount()=2
      maskna[j] = zmo
   endif
   j=j+1
   skip
enddo
return
*
proc ErrorTxt
para etxt
priv oldcol
oldcol=setcolor(ecol)
@ 24,0
@ 24,4 say etxt+' Нажмите ┘'
keyboard ''
inkey(0)
setcolor(oldcol)
@ 24,0
return
*
Procedure  MountDsk
parameters  dm,kspm,ok,test
priv i,fst,err,oldcol,wrk_scr
oldcol=setcolor(stdc)
wrk_scr=savescreen(5,10,9,70)
err=.f.
fst=test
do while .t.
   if fst
      fst=.f.
   else
      restscreen(5,10,9,70,wrk_scr)
      setcolor(bcol)
      @ 5,10,9,70 box frame+'░'
      keyboard ''
      setcolor(ecol)
      @ 7,12 say ' Установите на дисковод '+upper(dm)+' диск BASE'+kspm+' и нажмите ┘ '
      if err
         @ 8,12 say 'Вы ошиблись. Повторите установку или нажмите Esc-отказ...'
      endif
      i=0
      do while i<>13 .and. i<>27
         i=inkey(0)
      enddo
      if i=27
         ok=.f.
         exit
      endif
   endif
   if .not. test
      ok=.t.
      exit
   endif
   if file(dm-':BASE'-kspm-'.dbf')
      ok=.t.
      exit
   endif
   err=.t.
enddo
restscreen(5,10,9,70,wrk_scr)
setcolor(oldcol)
return
*
Procedure FReadSSP
Parameters nom,fld
priv i,myfld,oldcol
if nom=1
   return
endif
oldcol=setcolor(ecol)
i=4
ok=.t.
myfld=upper(trim(fld))
do case
 case (myfld=='D'.or.myfld=='M->DM')
    w=asc(upper(&myfld))
    if w<asc('A') .or. w>asc('Z') && .not. (upper(&myfld) $ 'ABCD')
       @ 24,0
       @ 24,5 say 'Допустимые адреса дисководов: A B C D ... Z '
       i=13
       ok=.f.
    endif
 case (myfld=='Z'.or.myfld=='M->ZM')
    if .not. (&myfld $ '+-')
       @ 24,0
       @ 24,5 say 'Признак закрепления: + (плюс) или - (минус) -- да/нет'
       i=13
       ok=.f.
    endif
endcase
if .not. ok
   @ 24,col() say '. Нажмите ┘'
   keyboard ''
   inkey(0)
endif
setcolor(oldcol)
keyboard chr(i)
return
*
Procedure VvodSSP
Parameter FirstLn,FileName,nomer,kolfld
priv    err
declare fields[kolfld]
do ReadFlds with FirstLn,FileName,nomer,kolfld
let_do=.t.
cr_read='FReadSSP'
do CallEdit with FirstLn,FileName,nomer,kolfld,;
                 fields,2,0,22,79,"ufunc",.t.,.t.
return
*
function  FreeCopy
parameter ifree,txt_f
priv i,wrk_scr,oldcol
oldcol=setcolor(stdc)
i=13
wrk_scr=savescreen(8,1,10,65)
do while ifree>diskspace()
   setcolor(bcol)
   @ 8,1,10,65 box frame+'░'
   setcolor(ecol)
   @ 8,3 say ' Нет места для копии '+txt_f
   @ 9,3 say ' Нажмите : ┘ - повторить, {Esc} - отказ '
   i=0
   do while i<>13 .and. i<>27
      i=inkey(0)
   enddo
   setcolor(bcol)
enddo
restscreen(8,1,10,65,wrk_scr)
setcolor(oldcol)
return if(i=13,.t.,.f.)
*
Function   UniqKodS
parameters fldn,kod,rec
priv i
loca for &fldn=kod .and. rec<>recno()
i=eof()
if found()
   do ErrorTxt with 'Вы ошиблись. Такой код уже имеетcя!!!'
endif
go rec
return i
*
Function   FldOkDZ
parameters procn,nom,fldn
ok=.f.
do &procn with nom,fldn
keyboard ''
return(ok)
*
Procedure SetMesP
priv mes_menu,txt_mes,oldtm,i,j,z1,z11,sr,dn1,z12,km1,z13
store 0 to z1,z11,sr,dn1,z12,z13
store ' ' to km1
sele 0
use pasp
save screen
oldtm=tm
mes_menu=val(tm)
sele 0
use srd
setcolor(stdc)
clear
TG=Tek_God
txt_mes=MesVybor('Установка')
sele pasp
if mes_menu<>0 .and. lastkey()<>27
   repl tm with substr(str(100+mes_menu,3),2,2)
   Tek_Mes_P=tm
   Tek_M_N=txt_mes
   setcolor(stdc)
   clear
   if val(tm)-val(oldtm)<0
      TG=str(val(Tek_God)+1,4)
      do TestYear with .t.,TG
      if lastkey()<>27
         Tek_God=TG
      endif
   endif
   sele pasp
   i=val(tm)+3  &&-8
   if i>12 &&<=0
      i=i-12
      TG=str(val(Tek_God)+1,4)
   endif
   setcolor(stdc)
   @ 19,0 clear
   sele srd
   go i
   setcolor(ucol)
   @ 20,1 say ' '+km+' '+naim+' '+TG+' '
   use  supersrd
   go top
   do while ! eof()
      tk=kod
      do CreatSrd with tk
      sele 0
      use srd&tk
      go i
      setcolor(bcol)
      @ 21,0,23,79 box framedbl+' '
      @ 21,1 say supersrd->kod+' -  '+supersrd->naim+' '+str(supersrd->prd)
      setcolor(ecol)
      set curs on
      @ 22,1 say ' Вводите кол-во рабочих дней ' get dn
      @ 22,col()+1 say ' и кол-во часов ' get chas
      @ 22,col()+1 say ' в этом месяце '
      read
      use
      set curs off
      sele supersrd
      skip
   enddo
   sele supersrd
   use
else
   sele srd
   use
endif
sele pasp
use
rest screen
return
*
Procedure ReadFlds
Parameter FirstLn,FileName,nomer,kolfld
sele 0
use &FileName alias edit_f
for if=1 to kolfld
 fields[if]=field(if)
 if fields[if]==''
  setcolor(ecol)
  @ 24,0
  @ 24,4 say 'Ошибка в количестве полей. Нажмите ┘ '
  keyboard ''
  if=inkey(0)
  setcolor(stdc)
  return
 endif
next
return
*
Procedure CallEdit
Parameter FirstLn,FileName,nomer,kolfld,fields,sr,sc,er,ec,ufunc,AppenDB,DeleRec
@ 1,0
@ 1,1 say FirstLn
sele edit_f
wrk_col=stdc
stdc=bcol
setcolor(stdc)
@ 23,0 clear
@ 0,0
keyboard chr(5)
Press1st=.t.
ln24=' {Esc}/{Ключ} - конец  {Del}/{УДЛ} - удаление строки  ┘ - заполнение поля'
DBEDIT(sr,sc, er, ec, fields, ufunc)
Press1st=.f.
stdc=wrk_col
setcolor(stdc)
sele edit_f
use
return
*
proc SSSB
para ss,sb
priv i,a,j,nu,wrk_dn,god,m1,oplats,tk
tk=TestKale()
DO CASE
 CASE PTS='1'
  a=1
 CASE PTS='2'
  a=chas_arr&tk[val(mespod)]
 case PTS='3'
  a=dn_arr&tk[val(mespod)]
 othe
  return
ENDC
GOD=tek_god
wrk_dn=a/100
for i=1 to 6
   nu=str(i,1)
   DO CASE
    CASE VAL(NHO&nu)=0
     D1='000000'
    CASE VAL(SUBS(NHO&nu,1,2))>50
     D1='19'+NHO&nu
    OTHE
     D1='20'+NHO&nu
   ENDC
   DO CASE
    CASE VAL(NKNO&nu)=0
     D2='999999'
    CASE VAL(SUBS(NKNO&nu,1,2))>50
     D2='19'+NKNO&nu
    OTHE
     D2='20'+NKNO&nu
   ENDC
   IF D1<=GOD+mespod.AND.D2>=GOD+mespod
       wrk=nvu&nu
       if wrk<>' '
          j=ascan(kodna,wrk)
          if j>0 .and. j<=len(kodna)
             m1=maskna[j]
             wrk_val_vo=val(wrk)
             do case
                CASE wrk_val_vo>=301.and.wrk_val_vo<=330
                  oplats=TestSvon(round(oks*a*nsu&nu/10000,0))
                othe
                  oplats=nsu&nu
             endc
             if substr(m1,8,1)='1'
                ss=ss+oplats
             endif
             if substr(m1,9,1)='1'
                sb=sb+oplats
             endif
          endif
       endif
   ENDIF
next
return
*
proc BlankRec
para base
repl kmn1 with '',vo1 with '',svon1 with 0,;
     kmn2 with '',vo2 with '',svon2 with 0,;
     kmn3 with '',vo3 with '',svon3 with 0,;
     kmn4 with '',vo4 with '',svon4 with 0,;
     kmn5 with '',vo5 with '',svon5 with 0,;
     kmn6 with '',vo6 with '',svon6 with 0,;
     kmn7 with '',vo7 with '',svon7 with 0,;
     kmn8 with '',vo8 with '',svon8 with 0,;
     kmn9 with '',vo9 with '',svon9 with 0,;
     kmn10 with '',vo10 with '',svon10 with 0,;
     kmn11 with '',vo11 with '',svon11 with 0,;
     kmn12 with '',vo12 with '',svon12 with 0,;
     cd with 0, oz with 0, kty with 0, kpm with 0
if M->base
   repl dn1 with 0,kmu1 with '',ku1 with '',sku1 with 0,;
        dn2 with 0,kmu2 with '',ku2 with '',sku2 with 0,;
        dn3 with 0,kmu3 with '',ku3 with '',sku3 with 0,;
        dn4 with 0,kmu4 with '',ku4 with '',sku4 with 0,;
        dn5 with 0,kmu5 with '',ku5 with '',sku5 with 0,;
        dn6 with 0,kmu6 with '',ku6 with '',sku6 with 0,;
        dn7 with 0,kmu7 with '',ku7 with '',sku7 with 0,;
        dn8 with 0,kmu8 with '',ku8 with '',sku8 with 0,;
        dn9 with 0,kmu9 with '',ku9 with '',sku9 with 0,;
        dn10 with 0,kmu10 with '',ku10 with '',sku10 with 0,;
        dn11 with 0,kmu11 with '',ku11 with '',sku11 with 0,;
        dn12 with 0,kmu12 with '',ku12 with '',sku12 with 0
endif
return
*
func AddNewK
para kod1,kod2,szp,p4,p5
priv ina,j,lst1,add0,rep0
if pcount()>3
   add0=p4
else
   add0=.f.
endif
if pcount()>4
   rep0=p5
else
   rep0=.t.
endif
if type('allna')='U'
   declare allna[12]
endif
 sele basef
 allna[1]=vo1
 allna[2]=vo2
 allna[3]=vo3
 allna[4]=vo4
 allna[5]=vo5
 allna[6]=vo6
 allna[7]=vo7
 allna[8]=vo8
 allna[9]=vo9
 allna[10]=vo10
 allna[11]=vo11
 allna[12]=vo12
 ina=scanna(allna,kod1,.f.,szp)
 if ina<1 .or. ina>12
    ina=scanna(allna,kod2,.t.,szp)
 endif
 if ina>0 .and. ina<13
    j=if(ina<10,str(ina,1),str(ina,2))
    if rep0 .or. szp<>0
       if szp=0 .and. .not. add0
          repl kmn&j with ' ', vo&j with ' ', svon&j with 0, dn&j with 0
       else
          repl kmn&j with mespod, vo&j with kod2, svon&j with szp
       endif
    endif
 endif
return ina
*
func ScanNa
para arr,kod,tst_val,uder_p,mesud
priv i,j,isp,oldcol,oldc,oldr
if pcount()<5
   i=ascan(arr,kod)
   mesud=mespod
else
   i=at('*'+kod+mesud,'*'+vo1+kmn1+'*'+vo2+kmn2+'*'+vo3+kmn3+'*'+vo4+kmn4+;
                      '*'+vo5+kmn5+'*'+vo6+kmn6+'*'+vo7+kmn7+'*'+vo8+kmn8+;
                      '*'+vo9+kmn9+'*'+vo10+kmn10+'*'+vo11+kmn11+'*'+vo12+kmn12)
   if i<>0
      i=int(i/(len(vo1)+len(km1)+1))+1
   endif
endif
if (i<1 .or. i>len(arr)) .and. tst_val .and. (uder_p<>0 .or. add0)
   i=ascan(arr, space(len(kod)))
   if i<1 .or. i>len(arr)
      declare aaa[12]
      aaa[1]=svon1
      aaa[2]=svon2
      aaa[3]=svon3
      aaa[4]=svon4
      aaa[5]=svon5
      aaa[6]=svon6
      aaa[7]=svon7
      aaa[8]=svon8
      aaa[9]=svon9
      aaa[10]=svon10
      aaa[11]=svon11
      aaa[12]=svon12
      i=ascan(aaa,0)
      rele aaa
   endif
endif
if i>0 .and. i<=len(arr)
   j=if(i<10,str(i,1),str(i,2))
   if rep0 .or. uder_p<>0
      if uder_p=0 .and. .not. add0
         repl kmn&j with ' ', vo&j with ' ', svon&j with 0
      else
         repl kmn&j with mesud, vo&j with kod, svon&j with uder_p
      endif
   endif
   arr[i]=vo&j
else
   if tst_val .and. (uder_p<>0 .or. add0)
      i=-1
      oldcol=setcolor(bcol)
      oldc=col()
      oldr=row()
      ScanScr=savescreen(10,20,15,79)
      @ 10,20,15,79 box framedbl+'░'
      @ 10,21 say super->fio
      if select('svo')<>0
         sele svo
         seek kod
      else
         naim=''
      endif
      setcolor(ecol)
      @ 12,22 say 'Нет cтроки для начисления '
      setcolor(stdc)
      @ 13,22 say kod+' '+if(found(),naim,'')+'.Меcяц '+mesud+;
                  '. Сумма '+str(uder_p/100)
      setcolor(ecol)
      @ 14,22 say ' Запомните его и нажмите ┘ '
      keyboard ''
      inkey(0)
      sele basef
      restscreen(10,20,15,79,ScanScr)
      setcolor(oldcol)
      @ oldr,oldc say ''
   endif
endif
return i
*
func TimeConv
para vrem,kind
priv i,j
if pcount()<2
   return vrem
endif
i=vrem/100
j=i-int(i)
if j=0
   return vrem
endif
do case
 case kind=1       &&  минуты в доли
   j=j/0.6
 case kind=2       &&  доли   в минуты
   j=j*0.6
endcase
return round((int(i)+j)*100,0)
*
Func TstKDolg
para a1,a2,na1,na2,r,c,naimenov,se_lo
priv ret_kod,txt_kod,seek_b
seek_b=.f.
sele &a2
if &a1.->&na1==space(len(&a1.->&na1))
  ret_kod=.t.
  txt_kod=space(len(&naimenov))
else
  if pcount()=8
     seek_b=if(upper(se_lo)='S',.t.,.f.)
  endif
  if seek_b
     seek &a1.->&na1
  else
     setcolor(ucol)
     @ 24,3 say '.....Ждите.....'
     loca for &a1.->&na1 = &na2
     setcolor(ecol)
     @ 24,0
     @ 24,3 SAY 'Вводите данные'
  endif
  ret_kod=found()
  if found()
     txt_kod=&naimenov
  else
     txt_kod='Ошибка'
  endif
  setcolor(stdc)
  sele &a1
  @ r,c say txt_kod
endif
sele &a1
return (ret_kod)
*
func TestSvon
para v,u
v=round_(v)
priv scr,ocol
if v>=99999999999
   Scr=savescreen(23,0,24,79)
   ocol=setcolor(stdc)
   @ 23,0 clear
   do ErrorTxt with 'Значение '+if(type('M->u')='N','удержания','начисления ')+;
                    ltrim(str(v/100))+' больше 99999999999'
   restscreen(23,0,24,79,Scr)
   setcolor(ocol)
   return 0
else
   return v
endif
*
func MesVybor
para txt_m
priv SprTop,SprNom,txt_mes,xb,yb,i
SprTop=1
SprNom=mes_menu
sele srd
set filter to recno()<13
xb=0
yb=0
do ListMenu with '',4,35,17,1,SprNom,SprTop,;
   '" "+km+". "+naim+" "',;
   ' {стрелки} - движение  {Esc}/{Kдюч} - конец  ┘/{Ввод} - выбор',;
   if(type('txt_m')="C",txt_m,'Архив'),'',xb,yb
set curs off
setcolor(stdc)
@ 24,0 clear
if SprNom=0
   mes_menu=0
   return ''
endif
sele srd
go   SprTop
skip SprNom - 1
mes_menu=SprNom
txt_mes=naim
if select('basef')<>0 .and. select('sebaf')<>0
   sele basef
   i=recno()
   sele sebaf
   go (i-1)*12+mes_menu
endif
return txt_mes
*
func PrSR
PARA MASKA
priv itsvo,vnnu,nnu,m1,j
itsvo=0
FOR VNNU=1 TO 12 
  NNU=ltrim(str(vnnu,2))
  j=ascan(kodna,vo&nnu)
  if j>0 .and. j<=len(kodna)
     if substr(maskna[j],maska,1)='1'
        itsvo=itsvo+svon&nnu
     endif
  endif
next
RETURN itsvo
*
proc DaDoPo
para p1
priv i
if tp_out<>2
   if prow()+6>Page_Size
     if pcount()>0
        do EndPage with p1
     else
        do EndPage
     endif
   endif
   type DaDoPo.sha
endif
return
*
Proc EndPage
para p1
priv pr_proc,old_c
if tp_out=0
   return
endif
pr_proc=if(pcount()>0,.t.,.f.)
if tp_out=1
   eject
endif
setprc(0,0)
if r_pech=1 .or. tp_out=2
   do ErrorTxt with ' Конец страницы '+alltrim(str(Page_Nom))+;
                    ' (для отказа от печати {Esc}/{Kлюч}) или ... '
   if lastkey()=27
      old_c=setcolor(ecol)
      @ 24,0
      @ 24,5 say '...Ждите...'
      setcolor(old_c)
      r_pech=0
      tp_out=0
      set print off
      set cons off
   endif
endif
setcolor(stdc)
@ 24,0
Page_Nom=Page_Nom+1
if tp_out=2
    clear
    @ 0,0 say ''
endif
if tp_out<>0
   if pr_proc
      do &p1
   endif
endif
return
*
proc TopRout
seek kodpod
return
*
Func TestKale
priv my_kalen,os_Tst,oc_Tst,scr_Tst
my_kalen=if(type('basef->kalendar')<>'C','',trim(basef->kalendar))
if type('dn_arr&my_kalen')<>"A"
   os_Tst=str(select())
   scr_Tst=savescreen(20,0,24,79)
   oc_Tst=setcolor(ecol)
   sele basef
   @ 20,0 clear to 24,79
   @ 21,5 say if(type('super->fio')="C",trim(super->fio),'')+' т.н. '+tn+;
              ': График работы '+my_kalen+' не найден '
   spr_obl='supersrd'
   if select('supersrd')=0
     T_K_close=.t.
     sele 0
     use supersrd
   else
     T_K_close=.f.
   endif
   @ 24,3 say 'Вводите код или нажмите: F1-помощь, {Esc}/{Kлюч}-отказ'
   keyboard ''
   setcolor(stdc)
   @ 23,0
   set key 28 to HelpKod
   r_h=23
   c_h=23
   txt_h='naim'
   @ r_h,2  say 'График работы'
   sele basef
   set curs on
   @ r_h,19 GET kalendar;
            valid(tstkdolg('basef',spr_obl,'kalendar','kod',r_h,c_h,txt_h))
   READ
   set key 28 to
   set curs off
   my_kalen=trim(basef->kalendar)
   my_kalen=if(type('dn_arr&my_kalen')<>"A",'',trim(basef->kalendar))
   if T_K_close
      sele supersrd
      use
   endif
   sele &os_Tst
   restscreen(20,0,24,79,scr_Tst)
   setcolor(oc_Tst)
endif
return M->my_kalen
*

