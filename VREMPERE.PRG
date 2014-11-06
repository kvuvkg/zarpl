para kodpod,namepod,mespod,mesname,dm
priv SprNom,SprTop,TypePere
dm_o=dm
kodpod_o=kodpod
namepod_o=namepod
mespod_o=mespod
sele ssp
stor iif(d=' ',0,asc(upper(left(d,1)))-asc('A')+1) to addrdisk, addrssp
rec_ssp=recno()
close data
if file('work.dbf')
   sele 0
   use work
   zap
   use
endif
vidy=space(recc())
count_vre=0
Vremenno=.t.
do while .t.
   dm=dm_o
   kodpod=kodpod_o
   namepod=namepod_o
   mespod=mespod_o
   close data
   do OpenABC with 'ABC'
   sele sebaf
   lsebaf=recsize()
   head_sebaf=header()
   sele basef
   lbasef=recsize()
   head_basef=header()
   if .not. Vremenno .and. file('work.dbf')
      sele 0
      use work
      set rela to kodpod+tn into super
      go top
      do while .not. eof()
         sele basef
         go   super->base_rec
         repl super->rpv with '0'
         repl rpv        with '0'
         do BlankRec with .t.
         i=recno()
         sele sebaf
         go (i-1)*12+1
         for i=1 to 12
            do BlankRec with .f.
            skip
         next
         sele work
         skip
      enddo
      sele work
      use
   endif
   sele super
   seek kodpod
   VremTop=iif(eof(),-999,recno())
   VremNom=1
   i=0
   count next 100000 while ksp=kodpod to i
   vidy=space(i)
   count_vre=0
   do while .t.
      sele basef
      addrdisk=addrssp
      do MainInfP
      addrdisk=0
      setcolor(ucol)
      @  8,1 say 'Можно отметить '+;
         ltrim(;
               str(;
                   min(;
                       int(;
                           (diskspace(0)-head_basef-if(vremenno,0,head_sebaf))/;
                           (lbasef+if(vremenno,0,lsebaf));
                          ),;
                        recc();
                      );
                  );
              )
      @ 10,1 say 'Отмечено:      '+ltrim(str(count_vre))
      open_a_b=.t.
      setcolor(mcol)
      xb=0
      yb='ksp<>kodpod'
      set key -5 to PressCr
      set key -7 to PressCr
      keyboard ''
      PressKey=0
      sele super
      do ListMenu with 'MakeVrem',1,40,22,1,VremNom,VremTop,;
         '" "+tn+" "+fio+" "',;
' {Esc}/{Kлюч}-отказ ┘/{Ввод}-выбор F6-перевести временно F8-перевести постоянно',;
         'Список сотрудников','',xb,yb,'TopRout'
      set key -7 to
      set key -5 to
      if VremNom=0
         count_vre=0
         exit
      endif
      if PressKey=-5 .or. PressKey=-7
         exit
      endif
      wrk = substr(vidy, base_rec , 1)
      vidy= stuff (vidy, base_rec , 1, iif(wrk='1','0','1'))
      count_vre=count_vre+iif(wrk='1',-1,1)
   enddo
   if count_vre=0
      exit
   endif
   Vremenno=if(PressKey=-5,.t.,.f.)
   sele basef
   if .not.FreeCopy(head_basef+lbasef+if(vremenno,0,lsebaf*12+head_sebaf),;
              '. Отметьте меньше работающих...')
      loop
   endif
   *куда
   sele 0
   use ssp
   SprNom=1
   set filter to z='+'.and.ksp<>kodpod
   go top
   SprTop=iif(eof(),-999,recno())
   set color to /
   clear
   setcolor(ucol)
   @ 0,0 say ' Перевод '+if(Vremenno,'временно','постоянно')+' из '
   @ 1,0 say ' '+kodpod+' '
   @ 2,0 say ' '+trim(namepod)+' '
   setcolor(stdc)
   keyboard ''
   xb=0
   yb=0
   do ListMenu with '',1,35,22,1,SprNom,SprTop,;
      'str(recno(),4)+". "+ksp+" "+nbrig+" "+d+" "+mes+" "',;
      '{клавиши-стрелки} - движение  {Esc}/{Ключ} - конец   ┘- выбор',;
      'Куда переводить ? ','',xb,yb
   if SprNom=0
      sele ssp
      use
      loop
   endif
   dm=d
   kodpod=ksp
   namepod=nbrig
   mespod=mes
   sele ssp
   stor iif(d=' ',0,asc(upper(left(d,1)))-asc('A')+1) to addrdisk
   use
   ok=.f.
   do MountDsk with dm,kodpod,ok,.t.
   if .not. ok
      dm=dm_o
      kodpod=kodpod_o
      namepod=namepod_o
      mespod=mespod_o
      loop
   endif
   sele basef
   copy to work for substr(vidy,recno(),1)='1'
   if ! vremenno
      sele sebaf
      copy to work1 for substr(vidy,int((recno()-1)/12)+1)='1'
   endif
   sele basef
   use
   sele sebaf
   use
   sele super
   use
   do OpenABC with 'ABC'
   sele sebaf
   lsebaf=recsize()
   sele basef
   lbasef=recsize()
   free_rec=0
   count all to free_rec for tn=replicate('*',len(tn))
   super_rec=0
   if FreeCopy(int((recsize()*(count_vre-free_rec))/4096)*4096,;
              '. Отметьте меньше работающих...')
      sele basef
      set rela to
      sele super
      use superlist index supertn,superpono alias super
      sele 0
      use work
      if ! vremenno
         sele 0
         use work1
      endif
      do while .not. eof()
         sele super
         seek work->tn
         newtn=work->tn
         newfio=fio
         sele basef
         do NewMan
         sele basef
         j=fcount()
         for i=1 to j
           wrk=field(i)
           repl &wrk with work->&wrk
         next
         repl rpv with if(Vremenno,'0','1')
         if Vremenno
            do BlankRec with .t.
         else
            sele basef
            rec_b=recno()
            sele sebaf
            go (rec_b-1)*12+1
            sele work
            rec_w=recno()
            go (rec_w-1)*12+1
            sele sebaf
            for i=1 to 12
               for j=1 to fcount()
                  wrk=field(j)
                  repl &wrk with work1->&wrk
               next
               sele work1
               skip
               sele sebaf
               skip
            next
         endif
         sele work
         skip
      enddo
      sele work
      use
      if select('work1')<>0
         sele work1
         use
      endif
   endif
   sele basef
   use
   sele sebaf
   use
   sele super
   use
enddo
dm=dm_o
kodpod=kodpod_o
namepod=namepod_o
mespod=mespod_o
close data
if file('work.dbf')
   erase work.dbf
endif
return
   
*
Procedure MakeVrem
Parameters MenuRout,x1,y1,x2,y2,lines,recs,textp,line24,top_txt,bot_txt,xb,yb
priv i
setcolor(ecol)
go recs
for i=x1 to xb-1
   @ i,y1-1 say iif(substr(vidy,base_rec,1)='1','+','')
   skip
next
setcolor(mcol)
return
*
proc NewMan
priv rep_rec
setcolor(ecol)
@ 24,0
@ 24,1 say '.....Ждите.....'
setcolor(stdc)
sele super
seek newtn
do while tn=newtn .and. ksp<>kodpod .and. ! eof()
   skip
enddo
rep_rec = ( ksp=kodpod .and. tn=newtn .and. ! eof() )
if rep_rec
   sele basef
   go super->base_rec
else
   sele super
   seek replicate('*',len(tn))
   if eof()
      append blank
   endif
   sele basef
   loca for tn=replicate('*',len(tn))
endif
if eof()
   append blank
   b_r=recno()
else
   j=fcount()
   b_r=recno()
   for i=1 to j
     wrk=field(i)
     repl &wrk with iif(type('&wrk')='C',' ',0)
   next
   i=recc()*12
   sele sebaf
   do while recc()<i
      append blank
   enddo
   sele basef
   i=(recno()-1)*12+1
   if Vremenno
      sele sebaf
      go i
      for j=0 to 11
        do BlankRec with .f.
        skip
      next
   endif
endif
sele super
super_rec=recno()
sele basef
i=recc()*12
sele super
repl rpv with if(Vremenno,'0','1'),;
     tn  with newtn,  fio      with newfio,;
     ksp with kodpod, base_rec with b_r
sele sebaf
do while recc()<i
   append blank
enddo
setcolor(stdc)
@ 23,0 clear
setcolor(ucol)
@ 23,17 say ' '+newtn+'. '+newfio+if(rep_rec,'заменен','добавлен')+' '
sele basef
return
