parameter SprParm,next_func
priv SprNom,SprTop
set color to &stdc
srow=3
scol=6
sele a
use &SprParm
SprNom=1
go top
SprTop=if(eof(),-999,recno())
do while .t.
 set color to &stdc
 clear
 imax=0
 set color to &mcol
 set key  7 to PressCr
 set key 22 to PressCr
 set key -2 to PressCr
 set key -3 to PressCr
 set key -4 to PressCr
 set key 084 to PressCr
 set key 116 to PressCr
 set key 073 to PressCr
 set key 105 to PressCr
 keyboard ''
 PressKey=0
 xb=0
 yb=0
 sele a
 do ListMenu with '',1,2,21,1,SprNom,SprTop,'trim(txt)+" "',;
' {Esc}/{КЛЮЧ} - конец  {0}/{ВСТ}/{Ins} - создание  {.}/{УДЛ}/{Del} - удаление   '+;
' F3 - печать справочника F5 - сортировка справочника ┘ - заполнение справочника',;
'Создание и Корректировка Справочников и Классификаторов','',xb,yb
 set color to &stdc
 @ 24,0
 set key  7 to
 set key 22 to
 set key -2 to
 set key -3 to
 set key -4 to
 set key 084 to
 set key 116 to
 set key 073 to
 set key 105 to
 if PressKey=84 .or. PressKey=116
    next_func=3
    exit
 endif
 if PressKey=73 .or. PressKey=105
    next_func=1
    exit
 endif
 if SprNom=0
    exit
 endif
 sele a
 go   SprTop
 skip SprNom - 1
 if PressKey<>0
  MyPress=PressKey
  do case
     case MyPress=7
        do SprDel
     case MyPress=22 .or. MyPress=-3
        do SprCrMod
     case MyPress=-2
        do SprPrint
     case MyPress=-4
        do SprSort
  endcase
  loop
 endif
 sele a
 go   SprTop
 skip SprNom - 1
 wrk   = proc           && SprProc[SprNom]
 s_col = colshap        && SprColS[SprNom]
 Shap  = trim(shapka)   && trim(SprShap[SprNom])
 txt1  = trim(txt)      && SprList[SprNom]
 file1 = file           && SprFile[SprNom]
 maxf1 = maxfldn        && SprMaxF[SprNom]
 let_do=.f.
 do &wrk with txt1,file1,SprNom,maxf1
 sele a
enddo
sele a
use
if file('sprwork.dbf')
   erase sprwork.dbf
endif
if file('sprwork.txt')
   erase sprwork.txt
endif
return
*
Procedure SprSort
priv fl
fl=file
sele 0
use &fl alias edit_f
if recc()>0
   set color to &ecol
   @ 24,0
   @ 24,21 say 'Идет сортировка'
   ind=field(1)
   index on &ind to work.ntx
   copy  to work.dbf
   zap
   appe from work
   use
   erase work.dbf
   erase work.ntx
   set color to &stdc
   @ 24,0
else
   use
endif
return
*
Procedure SprPrint
priv i,j,fl,lstf,nf
if upper(file)='DOPSVED'           && 19.04.89 09:18
   do DopSvedi with 0,0,0,0,.t.
   return
endif
if upper(file)='PASP'
   do PaspInpt with 0,0,0,0,.t.
   return
endif
set print on
set cons  off
sele a
? trim(txt)
? replicate('-',80)
? left(shapka,80)
? subs(shapka,81,80)
? replicate('-',80)
j=maxfldn
fl=file
sele 0
use &fl alias edit_f
lstf='" "'
for i=1 to j
   nf=field(i)
   lstf=lstf-'+" "+'-if(type('&nf')='N','str(&nf)','&nf')+'+" :"'
next
go top
do while .not. eof()
   ? &lstf
   skip
enddo
? replicate('-',80)
? ' '
sele edit_f
use
if trim(upper(fl))=='SVO' .and. file('sprobr.dbf')
       sele 0
       use sprobr
       ? ''
       ? replicate('-',80)
       ? ' Справочник масок обработки '
       ? replicate('-',80)
       ? ''
       list off str(recno(),3)+'.'+naim
       ? ''
       ? replicate('-',80)
       ? ''
       ? ' Примечание: 1 - маска включена, 0 - выключена '
       ? replicate('-',80)
       ? ''
       use
endif
set cons on
set print off
sele a
return
*
Procedure SprDel
Private r
r=' '
do AskYesNo with r,24,'Удалить строку ? ',ecol
if r='D'
   filen=trim(file)-'.dbf'
   @ 24,0
   @ 24,5 say 'Удаляем...'
   delete
   pack
   if file(filen)
      r=' '
      do AskYesNo with r,24,'Удалить файл тоже ? ',ecol
      if r='D'
         erase &filen
      endif
   endif
endif
set color to &stdc
@ 24,0
return
*
Procedure SprCrMod
    sele a
    modi_str=.t.
    if MyPress=22
       filen=space(len(file))
       tek_spr=-999
       Line1st=''
    else
       filen=file
       tek_spr=recno()
       Line1st=trim(txt)
       if filen=' '
          MyPress=3
       endif
    endif
    set color to &ecol
    @ 23,0 clear to 24,79
    do while .t.
      set curs on
      set color to &stdc
      @ 24,0
      @ 24,2 say 'Вводите имя файла или Esc-отказ ' get filen
      read
      set curs off
      if lastkey()=27
         exit
      endif
      if filen=space(len(filen))
         loop
      endif
      filen=upper(filen)
      if MyPress=-3
         set filter to recno()<>tek_spr
      endif
      locate for upper(file)==filen
      set filter to
      if eof()
         exit
      endif
      @ 23,0
      @ 23,1 say 'Файл '+filen+' используется для "'+trim(txt)+'"'
    enddo
    if lastkey()=27
      return
    endif
    copyfl=.f.
    if MyPress=-3 .or. (file(filen-'.dbf') .and. MyPress=22)
       MyPress = -3
       if file(filen-'.dbf')
          r=' '
          do AskYesNo with r,24,;
             'Будете модифицировать структуру файла ? ',ecol
          if r='D'
             modi_str=.t.
             sele b
             use &filen alias edit_f
             if field(1)==''
                MyPress=3
             else
                if recc()>0
                   copy to sprwork sdf deli
                   copyfl=.t.
                endif
             endif
          else
             modi_str=.f.
          endif
          @ 24,0
       else
          MyPress=3
       endif
    endif
    if MyPress=-3
       sele b
       use &filen alias edit_f
    else
       sele a
    endif
    copy to sprwork stru extended
    sele b
    use sprwork
    kolp=reccount()
    if modi_str
      if MyPress<>-3
        zap
      endif
      use
      let_do=.t.
      cr_read='TestType'
      s_col=16
      Shap=left('Имя поля     Тип поля    Длина поля   Кол. знаков   '+space(80),80)+;
           left('             (C или N)                после запятой '+space(80),80)
      do SprTriv with 'Структура файла '+trim(filen)+'. '+Line1st,'sprwork',0,4
      release let_do
      sele b
      use sprwork
      kolp=reccount()
      use
      if kolp=0
         return
      endif
      sele b
      create &filen from sprwork
      if copyfl      && MyPress=-3
         use &filen
         appe from sprwork sdf deli
      endif
    endif
    sele a
    if tek_spr>0 .and. tek_spr<=reccount()
       go tek_spr
    else
       appe blan
       repl proc with 'sprtriv'
    endif
    save screen
    set color to &stdc
    clear
    s1=left(shapka,80)
    s2=substr(shapka,81)
    txt1=txt
    proc1=proc
    cols1=colshap
    sele b
    use  sprwork
    @ 11,0 say ''
    sele b
    re_s=0
    go top
    for i=1 to kolp
       m=len(trim(field_name))
       j=max(field_len,m)
       L=j-m
       k=int(L/2)
       @ 11,col() say;
         replicate('-',1+k)+trim(field_name)+replicate('-',if(k*2=L,1,2)+k)+':'
       skip
       re_s=re_s+j+1
    next
    use
    sele a
    if cols1=0 .and. proc1='sprtriv'
       cols1=int((80-re_s)/2)
    endif
    set curs on
    set color to &stdc
    @ 1,1  say 'Файл '+filen
    @ 3,1  say 'Комментарий ' get txt1
    @ 5,1  say 'Процедура обработки  ' get proc1
    @ 6,1  say 'Колонка начала шапки ' get cols1
    @ 8,1  say 'Текст шапки '
    @ 9,0  get s1
    @ 10,0 get s2
    read
    set curs off
    rest screen
    sele a
    repl file with filen , maxfldn with kolp
    if lastkey()<>27 .and. updated()
      repl txt with txt1, proc with proc1, colshap with cols1, shapka with s1+s2
    endif
return
*
