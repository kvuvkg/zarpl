Parameter FirstLn,FileName,nomer,kolfld
priv    err, upd_svo
declare fields[kolfld]
upd_svo=.f.
let_do=.t.
cr_read='ReadMask'
do ReadFlds with FirstLn,FileName,nomer,kolfld
sele 0
use sprobr
SkvoTop=if(eof(),-999,recno())
SkvoNom=1
sele 0
use spsz
r_h=0
c_h=0
txt_h='""'
sele edit_f
clear
do CallEdit with FirstLn,FileName,nomer,kolfld,fields,2,0,22,70,"ufunc",.t.,.t.
sele sprobr
use
sele spsz
use
if upd_svo
   sele 0
   use svo
   index on kvoy to svoindex
   use
endif
release fields
return
*
Procedure ReadMask
Parameters nom,fld
if nom=2
   set key 28 to
   if fld='KVOY'
      upd_svo=updated() .or. upd_svo
   endif
   keyboard chr(4)
   return
endif
if fld='ZMO'
   setcolor(ecol)
   @ 24,0
   @ 24,1 say 'Вводите маcку обработки или F1-помощь'
   set key 28 to HelpMask
elseif fld='KPS'
   setcolor(ecol)
   @ 24,0
   @ 24,1 say 'Вводите код вида затрат или F1-помощь'
   set key 28 to HelpKod
else
   return
endif
return
*
proc HelpMask
PARA pn, pl, fld
set key 28 to
sele edit_f
stor left(trim(&fld)+replicate('0',len(&fld)),len(&fld)) to vidy, old_vidy
rm_col=col()
rm_row=row()
sele sprobr
set filter to recno()<=len(vidy)
keyboard ''
if len(vidy)<=22
 declare field1[1]
 field1[1]='naim'
 Press1st=.t.
 keyboard chr(5)
 my_left=74-len(naim)
 rm=savescreen(0,my_left,24,79)
 setcolor(bcol)
 @ 0,my_left,24,79 box replicate('░',9)
 setcolor(mcol)
 dbedit(0, my_left+2, 23, 78, field1, "vidylist") &&, .T., .T., .T., .T., "═╧═")
 setcolor(stdc)
else
 my_left=51
 rm=savescreen(0,51,24,79)
 xb=0
 yb=0
 do while .t.
    do ListMenu with 'MakeOper',1,52,23,1,SkvoNom,SkvoTop,;
                  'left(str(recno(),2)+"."+naim,26)',;
    ' {Kлавиши-стрелки} - движение  {Esc}/{Kлюч} - конец  ┘/{Ввод} - выбор',;
                  'Маски обработки','',xb,yb
    if SkvoNom=0
       exit
    endif
    wrk = substr(vidy, SkvoTop + SkvoNom -1 , 1)
    vidy= stuff (vidy, SkvoTop + SkvoNom -1 , 1,;
                       if(wrk='1','0','1'))
 enddo
endif
set filter to
restscreen(0,my_left,24,79,rm)
rele rm
sele edit_f
repl &fld with vidy
@ rm_col,rm_row say ''
keyboard chr(13)
set key 28 to HelpMask
return
*
FUNCTION VidyList
PARAMETERS mode,i_cur
priv i,wrk
if Press1st
 setcolor(bcol)
 @ 0,my_left,0,79 box replicate('░',9)
 @ 1,my_left+1 say 'Маски обработки'
 setcolor(ecol)
 @ 24,0
 @ 24,1 say '^U - восстановление масок ┘/{Ввод} - переключение маски  {Esc}/{Kлюч} - конец'
 vidy=old_vidy
 for i=1 to len(vidy)
   @ i+1,my_left+1 say if(substr(vidy,i,1)='1','+',' ')
 next
 setcolor(mcol)
 Press1st=.f.
endif
if lastkey()=27
   return 0
endif
if mode=4
 if lastkey()=13
   setcolor(ecol)
   wrk = substr(vidy, recno() , 1)
   vidy= stuff (vidy, recno() , 1, if(wrk='1','0','1'))
   @ row(),my_left+1 say if(substr(vidy,recno(),1)='1','+',' ')
   setcolor(mcol)
 elseif lastkey()=21   && ^U
   Press1st=.t.
   keyboard chr(19)
 endif
endif
RETURN 1
*
Procedure MakeOper
Parameters MenuRout,x1,y1,x2,y2,lines,recs,textp,line24,top_txt,bot_txt,xb,yb
priv i
setcolor(ecol)
for i=x1 to xb-1
   @ i,y1-1 say if(substr(vidy,recs+i-x1,1)='1','+','')
next
setcolor(mcol)
return
*
Procedure SprSuder
Parameter FirstLn,FileName,nomer,kolfld
priv    err, upd_suder
declare fields[kolfld]
upd_suder=.f.
let_do=.t.
cr_read='TestSude'
do ReadFlds with FirstLn,FileName,nomer,kolfld
sele edit_f
clear
do CallEdit with FirstLn,FileName,nomer,kolfld,fields,2,0,22,70,"ufunc",.t.,.t.
if upd_suder
   sele 0
   use suder
   index on kvoy to suderind
   use
endif
release fields
return
*
Procedure TestSude
Parameters nom,fld
if nom=2
   if fld='KVOY'
      upd_suder=updated() .or. upd_suder
   endif
   keyboard chr(4)
endif
return
*
proc PaspInpt
Parameter FirstLn,FileName,nomer,kolfld,print_pr
priv pri
if pcount()<5
   pri=.f.
else
   pri=print_pr
endif
sele 0
use pasp
if eof()
   append blank
endif
if pri
   set device to print
   setprc (0,0)
else
   setcolor(bcol)
   @ 0,0,24,79 box frame+'░'
endif
setcolor(ecol)
@ 2,10 say ' П а с п о р т   п р е д п р и я т и я '
setcolor(stdc)
getcol=31
@  4,5 say ' Код предприятия          '+kp
@  5,5 say ' Наименование предприятия '+naim
@  6,5 say ' Расчетный счет           '+rs
@  7,5 say ' Код банка                '+kb
@  8,5 say ' Отделение банка          '+ob
@ 15,0 say ''
set device to screen
if ! pri
   set curs on
   @  4,getcol  get kp
   @  5,getcol  get naim
   @  6,getcol  get rs
   @  7,getcol  get kb
   @  8,getcol  get ob
   read
   set curs off
endif
name_pred=naim
use
setcolor(ecol)
@ 24,0
@ 24,5 say '....Ж д и т е....'
setcolor(stdc)
do ProtCT
return
*
proc DopSvedi
Parameter FirstLn,FileName,nomer,kolfld,print_pr
priv pri
if pcount()<5
   pri=.f.
else
   pri=print_pr
endif
sele 0
use sorg
sele 0
use dopsved
if eof()
   append blank
endif
if pri
   set device to print
   setprc (0,0)
else
   setcolor(bcol)
   @ 0,0,23,79 box frame+'░'
endif
setcolor(ecol)
@ 2,10 say ' ДОПОЛНИТЕЛЬНЫЕ СВЕДЕНИЯ '
setcolor(stdc)
getcol=41
@  4,5 say ' Горжилуправление ................. '+;
           ukogju+' '+loca_org(ukogju,4,5,50,.f.)
@  5,5 say ' Горсвет .......................... '+;
           ukosv +' '+loca_org(ukosv,5,5,50,.f.)
@  6,5 say ' Городская АТС .................... '+;
           ukotel+' '+loca_org(ukotel,6,5,50,.f.)
@  7,5 say ' Главпочтампт ..................... '+;
           ukopoch+' '+loca_org(ukopoch,7,5,50,.f.)
@ 11,5 say ' Отчисления на соцстрах ........... '+str(sos,3)
@ 12,5 say ' Резерв отпусков .................. '+str(ro ,3,1)
@ 14,5 say ' Мин.размер пособия на 1 ребенка .. '+str(minra,3)
if type('maxs')='N'
   @ 15,5 say ' Макс.сумма для начисления северных '+str(maxs)
endif
if type('maxr')='N'
   @ 16,5 say ' Макс.сумма для начисления районных '+str(maxr)
endif
if type('rk')='N'
   @ 17,5 say ' Районный коэффициент ............. '+str(rk)
endif
set device to screen
if ! pri
   line24=;
'Вводите данные ( F1-помощь {Esc}/{Ключ}-отказ ┘/{Ввод}-переход к сл.полю)'
   set curs on
   set key 28 to HelpOrg
   setcolor(ecol)
   @ 24,0
   @ 24,1 say line24
   setcolor(stdc)
   korrf='sorg'
   @  4,getcol  get ukogju   valid loca_org(ukogju ,4,5,50,.t.,line24)
   @  5,getcol  get ukosv    valid loca_org(ukosv  ,5,5,50,.t.,line24)
   @  6,getcol  get ukotel   valid loca_org(ukotel ,6,5,50,.t.,line24)
   @  7,getcol  get ukopoch  valid loca_org(ukopoch,7,5,50,.t.,line24)
   @ 11,getcol  get sos
   @ 12,getcol  get ro
   @ 14,getcol  get minra
   if type('maxs')='N'
      @ 15,getcol  get maxs
   endif
   if type('maxr')='N'
      @ 16,getcol  get maxr
   endif
   if type('rk')='N'
      @ 17,getcol  get rk
   endif
   read
   set curs off
   prod_rd  =prd
   Min_Raz_A=minra
   DOPS_MAXS=if(type('maxs')='N',maxs*100,0)
   DOPS_MAXR=if(type('maxr')='N',maxr*100,0)
   DOPS_RK  =if(type('rk')='N'  ,rk      ,0)
endif
sele dopsved
use
sele sorg
use
return
*
function Loca_Org
para     kod,r_h,s_h,c_h,help_prm,txt_24
priv     tek_obl,n_org,ret_prm,old_c
tek_obl=select()
old_c=setcolor(ecol)
if type('txt_24')="C"
   @ 24,0
   @ 24,1 say '...Ждите...'
endif
sele sorg
loca for koo==kod
@ r_h,c_h say if(eof(),'---код неверен---',naim)
@ r_h,s_h say ''
if type('txt_24')="C"
   @ 24,0
   @ 24,1 say txt_24
endif
setcolor(old_c)
ret_prm=if(eof(),.f.,.t.)
sele (m->tek_obl)
return if(help_prm,ret_prm,'')
*
proc HelpOrg
PARA pn, pl, fldp
priv xb,yb,HelpNom,HelpTop,tek_obl,wrk_scr
set key 28 to
tek_obl=select()
xb=0
yb=0
sele &korrf
go top
HELPTop=if(eof(),-999,recno())
HELPNom=1
wrk_scr=savescreen(0,35,23,79)
@ 0,35,23,79 box replicate('░',9)
PressKey=0
do ListMenu with '',2,37,22,1,HELPNom,HELPTop,"' '+koo+' '+naim+' '",;
   ' {Kлавиши-стрелки} - движение  {Esc}/{Kлюч} - конец  ┘/{Ввод} - выбор',;
          'Справочник организаций','',xb,yb
sele (tek_obl)
keyboard if(HELPNom=0,'',&korrf->koo)
restscreen(0,35,23,79,wrk_scr)
set key 28 to HelpOrg
return
*
proc InptPoAl
Parameter FirstLn,FileName,nomer,kolfld
priv    err
declare fields[2]
let_do=.t.
cr_read='ReadPoAl'
kolfld=2
do ReadFlds with FirstLn,FileName,nomer,kolfld
clear
do CallEdit with FirstLn,FileName,nomer,kolfld,fields,2,0,22,70,"ufunc",.t.,.t.
return
*
Procedure ReadPoAl
Parameters nom,fld
priv scr,oldcol
if fld=fields[1]
   return
endif
if nom=1
   keyboard chr(13)
   return
endif
Scr=savescreen(9,0,18,79)
@ 9,0,18,79 box framedbl+' '
oldcol=setcolor(ecol)
@ 24,0
@ 24,3 say 'Вводите'
setcolor(stdc)
set curs on
@ 10,0 clear to 17,79
@ 11,2 say " Код     "+kap
@ 13,2 say " Ф.И.О. " get fia
@ 15,2 say " Адрес  " get apa && pict repl("X",len(apa))
read
set curs off
@ 24,0
restscreen(9,0,18,79,Scr)
return
*
Procedure SprKalen
Parameter FirstLn,FileName,nomer,kolfld
priv    err, upd_kalen
declare fields[kolfld]
upd_kalen=.f.
sele 0
use supersrd
stor 1 to SkvoNom
SkvoTop=if(eof(),-999,recno())
xb=0
yb=0
do while .t.
   sele supersrd
   set key 22 to PressCr
   set key  7 to PressCr
   set key -3 to PressCr
   set key -4 to PressCr
   keyboard ''
   PressKey=0
   setcolor(stdc)
   clear
   do ListMenu with '',3,10,22,1,SkvoNom,SkvoTop,;
                 '" "+kod+". "+naim+" ("+str(prd)+") "',;
' {Esc}/{Kлюч}-конец {ВСТ}/{Ins}-создание {УДЛ}/{Del}-удаление ┘/{Ввод}-выбор   '+;
' F4 - корректировка описания   F5 - указание текущего года ',;
                 'Графики работы на '+Tek_God+' год','',xb,yb
   set key 22 to
   set key  7 to
   set key -3 to
   set key -4 to
   if SkvoNom=0
      exit
   endif
   go SkvoTop
   skip SkvoNom-1
   if PressKey=22 .or. PressKey=-3
       wrk_r=recno()
       tk=kod
       test='wrk<>" "'
       if PressKey=22
          wrk=' '
          wrk1=space(len(naim))
          wrk2=0
       else
          set filter to recno()<>wrk_r
          wrk=kod
          if wrk=' '
             test='wrk=" "'
          endif
          wrk1=naim
          wrk2=prd
       endif
       go top
       all_graf=''
       do while ! eof()
          all_graf=all_graf+kod
          skip
       enddo
       set filter to
       go wrk_r
       setcolor(ecol)
       @ 24,0
       @ 24,5 say 'Вводите'
       setcolor(stdc)
       @ 1,0 clear to 23,79
       set curs on
       @ 3,5 say ' Kод графика.................... ' get wrk;
             valid ! (wrk $ all_graf) .and. &test
       @ 5,5 say ' Hаименование................... ' get wrk1
       @ 7,5 say ' Продолжительность раб.дня (ч.м) ' get wrk2 pict '##.##'
       read
       set curs off
       if lastkey()<>27
          if PressKey=22
             appe blank
          endif
          repl kod with wrk, naim with wrk1, prd with wrk2
          do CreatSrd with wrk
          upd_kalen=.t.
       endif
   elseif PressKey=7
       if supersrd->kod<>' '
          r_del=' '
          do AskYesNo with r_del,24,kod+'. '+naim+': удалить ? ',ecol
          if r_del='D'
             wrk=kod
             delete
             pack
             erase srd&wrk..dbf
             go top
             SkvoTop=if(eof(),-999,recno())
             upd_kalen=.t.
          endif
       endif
   elseif PressKey=-4
       sele 0
       use srd
       TG=str(val(Tek_God),4)
       do TestYear with .t.,TG
       if lastkey()<>27
          Tek_God=TG
          go 13
          repl naim with Tek_God
       endif
       use
       upd_kalen=updated() .or. upd_kalen
   else
       tk=supersrd->kod
       sele 0
       do CreatSrd with tk
       sele 0
       use srd&tk alias edit_srd
       setcolor(stdc)
       clear
       setcolor(ucol)
       @ 0,0 say ' '+supersrd->kod+'. '+supersrd->naim+' ('+str(supersrd->prd)+') '
       setcolor(stdc)
       go top
       for i=1 to 12
          @ 3+i,2 say naim+str(dn,2)+' дн.'+str(chas,8,2)+' час.мин '
          skip
       next
       rrr=1
       do while .t.
          setcolor(mcolbln)
          @  3,0 prompt '*'
          @  4,0 prompt '*'
          @  5,0 prompt '*'
          @  6,0 prompt '*'
          @  7,0 prompt '*'
          @  8,0 prompt '*'
          @  9,0 prompt '*'
          @ 10,0 prompt '*'
          @ 11,0 prompt '*'
          @ 12,0 prompt '*'
          @ 13,0 prompt '*'
          @ 14,0 prompt '*'
          @ 15,0 prompt '*'
          setcolor(stdc)
          @ 3,2 say 'Продолжительность раб.дня (ч.м) '+;
                    str(supersrd->prd,5,2)+' '
          setcolor(ecol)
          @ 24,0
          @ 24,0 say;
    ' {Kлавиши-стрелки} - движение  {Esc}/{Kлюч} - конец  ┘/{Ввод} - выбор'
          setcolor(mcolbln)
          menu to rrr
          if rrr=0
             exit
          endif
          setcolor(ecol)
          @ 24,0
          @ 24,5 say 'Вводите'
          setcolor(stdc)
          set curs on
          if rrr=1
             @ 3,35 get supersrd->prd pict '##.##'
             read
          else
             go rrr-1
             @ 2+rrr,10 get dn
             @ 2+rrr,18 get chas
             read
             @ 2+rrr,2 say naim+str(dn,2)+' дн.'+str(chas,8,2)+' час.мин '
          endif
          set curs off
          upd_kalen=updated() .or. upd_kalen
          @ 24,0
          rrr=rrr+1
       enddo
       sele edit_srd
       use
   endif
enddo
sele supersrd
use
if upd_kalen
   setcolor(ecol)
   @ 24,0
   @ 24,5 say '.....Ж д и т е.....'
   do ReadSRDs
   setcolor(stdc)
   @ 24,0
endif
return
*
proc SDomAdI
Parameter FirstLn,FileName,nomer,kolfld
priv    err, open_sup, ndx1, ndx2
declare fields[kolfld+1]
if select('super')=0
   sele 0
   open_sup=.t.
else
   sele super
   open_sup=.f.
endif
use superlist index supertn alias super
let_do=.t.
cr_read='ReadDomA'
do ReadFlds with FirstLn,FileName,nomer,kolfld
for i=kolfld+1 to 3 step -1
  fields[i]=fields[i-1]
next
fields[2]='SUPER->FIO'
r_h=0
c_h=0
txt_h='""'
sele edit_f
set rela to tn into super
clear
do CallEdit with FirstLn,FileName,nomer,kolfld+1,fields,2,0,22,70,"ufunc",.t.,.t.
sele super
if open_sup
   use
else
   use superlist index superpono, supertn alias super
endif
return
*
Procedure ReadDomA
Parameters nom,fld
priv scr,oldcol
if 'FIO' $ upper(fld) .and. nom=1
   keyboard chr(13)
endif
if upper(fld)='TN' .and. nom=1
   set key 28 to HelpKod
   setcolor(ecol)
   @ 24,1 say 'Вводите маcку обработки или F1-помощь'
elseif nom=2
   set key 28 to
   keyboard chr(4)
endif
return
*
proc InptSorg
Parameter FirstLn,FileName,nomer,kolfld
priv    err
declare fields[3]
let_do=.t.
cr_read='ReadSorg'
kolfld=3
do ReadFlds with FirstLn,FileName,nomer,kolfld
clear
do CallEdit with FirstLn,FileName,nomer,kolfld,fields,2,0,22,70,"ufunc",.t.,.t.
return
*
Procedure ReadSorg
Parameters nom,fld
priv scr,oldcol
if fld<>fields[3]
   return
endif
if nom=1
   keyboard chr(13)
   return
endif
Scr=savescreen(9,0,18,79)
@ 9,0,18,79 box framedbl+' '
oldcol=setcolor(ecol)
@ 24,0
@ 24,3 say 'Вводите'
set curs on
setcolor(stdc)
@ 10,0 clear to 17,79
setcolor(ucol)
@ 11,2 say ' '+koo+' '+trim(naim)+' '
setcolor(stdc)
@ 12,2 say " Код банка       " get kb
@ 14,2 say " Расчетный счет  " get rs
@ 16,2 say " Отделение банка " get ob
read
set curs off
@ 24,0
restscreen(9,0,18,79,Scr)
return
*
Procedure SprTriv
Parameter FirstLn,FileName,nomer,kolfld
declare fields[kolfld]
do ReadFlds with FirstLn,FileName,nomer,kolfld
do CallEdit with FirstLn,FileName,nomer,kolfld,fields,2,0,22,79,"ufunc",.t.,.t.
release fields
return
*
FUNCTION ufunc
PARAMETERS mode,i_cur
PRIVATE cur_field,i,j_rec,new_val,www
public upd_sprm
i=i_cur
cur_field = fields[i]
keystroke = LASTKEY()
IF CHR(M->keystroke) $ M->move_chrs &&.OR. M->force_rec
  linestat()
ENDIF
if Press1st
 @ sr,sc clear to sr+1,ec
 jw=ec-sc+1  && 80-s_col
 if len(shap)<=jw
    @ sr,0 say;
'0----5---10----*---20----*---30----*---40----*---50----*---60----*---70----*--79'
    @ sr+1,s_col say Shap
 else
    @ sr,s_col say left(shap,jw)
    @ sr+1,s_col say substr(shap,81,jw)
 endif
 Press1st=.f.
 setcolor(ecol)
 @ 24,0
 @ 24,1 say ln24
 setcolor(stdc)
 linestat()
endif
DO CASE
    case mode=0
      return(1)
    case mode=2  .and. lastkey()=24 .and. AppenDB
      ie=recno()
      go bott
      zero=.t.
      do TestZero
      if zero
        keyboard ''
      else
        append blank
      endif
      return(1)
    case mode=3 .and. AppenDB
      append blank
      return(1)
    CASE mode <> 4
        RETURN(1)
    CASE LASTKEY() = 27
        go bott
        zero=.t.
        do TestZero
        if zero
         delete
         pack
        endif
        RETURN(0)
    CASE LASTKEY() = 13;
         .or. (lastkey()>=48 .and. lastkey()<=241 .and. lastkey()<>127)
        if lastkey()<>13
           keyboard chr(lastkey())
        endif
        priv i_r,i_c
        i_r=row()
        i_c=col()
        set color to
        @ 24,0
        setcolor(mcol)
        if let_do
         do &cr_read with 1,cur_field
        endif
        do while .t.
           sele edit_f
           set key 22 to tog_insert
           setcolor(ecol)
           @ 23,0 clear to 23,ec
           @ 23,10 say 'Вводите'
           setcolor(mcol)
           set key 22 to tog_insert
           set curs on
           @ i_r,i_c GET &cur_field
           READ
           set curs off
           set key 22 to
           @ 23,0 clear to 23,ec
           upd_sprm=updated() .or. upd_sprm
           if lastkey()=27
              exit
           endif
           if i_cur=1
              oldcol=setcolor(ecol)
              @ 24,0
              @ 24,5 say '...Kонтроль кода...'
              setcolor(oldcol)
              new_val=upper(&cur_field)
              j_rec=recno()
              loca all for upper(&cur_field) == new_val .and. recno()<>j_rec
              if eof()
                 go j_rec
                 exit
              else
                 www=fields[2]
                 do ErrorTxt with;
                    left('Код занят: '+;
                       if(type('&cur_field')="N",str(&cur_field),&cur_field)+;
                       ' '+if(type('&www')="N",str(&www),&www),65)
              endif
              go j_rec
           else
              exit
           endif
        enddo
        rele i_r,i_c
        setcolor(stdc)
        RetUfunc=1
        if let_do
           do &cr_read with 2,cur_field
        else
           KEYBOARD CHR(4)
        endif
        sele edit_f
        setcolor(ecol)
        @ 24,0
        @ 24,1 say ln24
        setcolor(stdc)
        return (RetUfunc)
    CASE LASTKEY() = 7 .and. DeleRec
        setcolor(ecol)
        @ row(),col() say &cur_field
        @ 24,0
        r=' '
        do AskYesNo with r,24,'Удалить строку ? ',ecol
        if r='D'
         @ 24,0
         @ 24,5 say 'Удаляем...'
         old_r=recno()
         delete
         pack
         if old_r>0 .and. old_r<=recc()
            go old_r
         else
            go bott
         endif
        endif
        setcolor(ecol)
        @ 24,0
        @ 24,0 say ln24
        linestat()
        setcolor(stdc)
        return(2)
    OTHERWISE
        * don't quit
        RETURN(1)
ENDCASE
*
Procedure TestZero
     zero=.t.
     lf=len(fields)
     for iw=1 to lf
      c_f=fields[iw]
      if .not. empty(&c_f)
         zero=.f.
         iw=lf+1
      endif
     next
return
*
Procedure TestType
Parameters nom,fld
if nom<>2
   return
endif
priv i         && заносим значение для KeyBoard
i=4            && сдвиг вправо
if fld='FIELD_TYPE'
  do case
   case &fld $ 'cn'
      repl &fld with upper(&fld)
   case .not. (&fld $ 'CN')
      @ 24,0
      @ 24,5 say 'Тип поля: C - символьное  N - числовое '
      i=13       && повторяем ввод
  endcase
endif
keyboard chr(i)
return
*
FUNCTION tog_insert
READINSERT(.NOT. READINSERT())
show_insert()
RETURN 0
FUNCTION show_insert
priv oldcol
oldcol=setcolor(ucol)
@ 0,45 SAY IF(READINSERT(), "<Вставка>", "<Замена> ")
setcolor(oldcol)
RETURN 0
FUNCTION linestat
priv oldcol
oldcol=setcolor(ucol)
@ 0,54
show_insert()
IF LASTREC() = 0 &&.AND. .NOT. M->is_append
   @ 0,55 SAY "<нет записей>"
ELSE
        IF EOF()
           @ 0,55 SAY "         "
        ELSE
           @ 0,55 SAY IF(DELETED(), "<Удалена>", "         ")
           @ 0,65 SAY;
                  pad(LTRIM(STR(RECNO())) + "/" + LTRIM(STR(LASTREC())),15)
        ENDIF
ENDIF
force_rec = .F.
setcolor(oldcol)
RETURN 0
FUNCTION lpad
PARAMETERS string,n
RETURN (SPACE(M->n - LEN(M->string)) + M->string)
