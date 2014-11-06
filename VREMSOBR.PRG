para kodpod,namepod,mespod,mesname,dm
priv SprNom,SprTop
set color to /
clear
dm_o=dm
kodpod_o=kodpod
namepod_o=namepod
mespod_o=mespod
close data
set color to &ucol
@ 0,0 say ' Переписываем данные временно переведенных '
sele 0
use vrempere
zap
sele 0
use superlist index superpono alias super
set filter to rpv='0'
go top
if eof()
   close data
   return
endif
sele 0
use ssp
stor if(d=' ',0,asc(upper(left(d,1)))-asc('A')+1) to addrdisk, addrssp
index on ksp to ssp
sele super
do while .not.eof()
   kodpod=ksp
   sele ssp
   seek kodpod
   if eof()
      sele super
      skip
      loop
   endif
   stor if(d=' ',0,asc(upper(left(d,1)))-asc('A')+1) to addrdisk
   dm=d
   kodpod=ksp
   namepod=nbrig
   mespod=mes
   ok=.f.
   do MountDsk with dm,kodpod,ok,.t.
   if .not. ok
      sele super
      skip
      loop
   endif
   do OpenABC with 'A','000'
   sele super
   do while ksp=kodpod .and. .not.eof()
      set color to &ucol
      @ 23,0 say ' Выборка '
      @ 24,0 say ' '+super->ksp+' '+super->tn+' '+trim(super->fio)+' '
      super_rec=recno()
      sele basef
      go super->base_rec &&& seek super->tn+super->rpv
         sele super
         set index to supertn
         set filter to rpv='1'
         seek basef->tn
         if .not. eof()
            podr_o=ksp
            sele basef
            for i=1 to 12
               nu=ltrim(str(i,2))
               if vo&nu<>space(len(vo&nu)) .and. svon&nu<>0
                  sele vrempere
                  append blank
                  replace o_ksp    with podr_o,;
                          o_tn     with basef->tn,;
                          o_km     with basef->kmn&nu,;
                          o_vo     with basef->vo&nu,;
                          o_dn     with basef->dn&nu,;
                          o_svon   with basef->svon&nu,;
                          o_base_r with super->base_rec
                  sele basef
               endif
            next
         endif
         sele super
         set filter to rpv='0'
         set index to superpono  && supervre
      sele super
      go super_rec
      skip
      set color to &stdc
      @ 23,0 clear
   enddo
   sele basef
   use
   sele super
enddo
set color to &ucol
@ 24,10 say '.....Ж Д И Т Е.....'
sele vrempere
if recc()>0
   index on o_ksp+o_tn to vrempere
   go top
   set color to &stdc
   @ 24,0
   do while .not. eof()
      kodpod=o_ksp
      sele ssp
      seek kodpod
      if eof()
         sele vrempere && super
         skip
         loop
      endif
      stor if(d=' ',0,asc(upper(left(d,1)))-asc('A')+1) to addrdisk
      dm=d
      kodpod=ksp
      namepod=nbrig
      mespod=mes
      ok=.f.
      do MountDsk with dm,kodpod,ok,.t.
      if ok
         do OpenABC with 'A','000'
         sele vrempere
         do while o_ksp=kodpod .and. .not. eof()
            set color to &ucol
            @ 23,0 say ' Разноска '
            @ 24,0 say ' '+o_ksp+' '+o_tn+' '
               sele basef
               go vrempere->o_base_r
               sele vrempere
               kod_p=o_ksp
               tn_p =o_tn
               i=0
               do while o_ksp=kod_p .and. o_tn=tn_p .and. .not. eof()
                  sele basef
                  i=i+1
                  do while i<13
                     nu=ltrim(str(i,2))
                     if vo&nu=' '
                        exit
                     endif
                     i=i+1
                  enddo
                  if i>12
                     exit
                  endif
                  
                  repl kmn&nu with vrempere->o_km, vo&nu with vrempere->o_vo,;
                       dn&nu  with vrempere->o_dn, svon&nu with vrempere->o_svon
                  sele vrempere
                  skip
               enddo
               sele vrempere
               if o_ksp=kod_p .and. o_tn=tn_p
                  if select('svo')=0
                     sele 0
                     use svo index svoindex
                  endif
                  sele vrempere
                  set rela to o_ksp+o_tn into super
                  do while o_ksp=kod_p .and. o_tn=tn_p .and. .not. eof()
                     do ErrNoSpc
                     sele vrempere
                     skip
                  enddo
                  set rela to
               endif
            sele vrempere
            set color to &stdc
            @ 23,0 clear
         enddo
         sele basef
         use
         sele vrempere
      else
         sele vrempere
         do while o_ksp=kodpod .and. .not. eof()
            skip
         enddo
      endif
   enddo
endif
sele super
set filter to
use
sele vrempere
use
sele ssp
use
if select('svo')<>0
   sele svo
   use
endif
if file('vrempere.ntx')
   erase vrempere.ntx
endif
if file('basef.ntx')
   erase basef.ntx
endif
if file('ssp.ntx')
   erase ssp.ntx
endif
if file('supervre.ntx')
   erase supervre.ntx
endif
dm=dm_o
kodpod=kodpod_o
namepod=namepod_o
mespod=mespod_o
return
*
proc ErrNoSpc
priv ScanScr
      ScanScr=savescreen(10,20,15,79)
      set color to &bcol
      @ 10,20,15,79 box framedbl+'░'
      @ 10,21 say super->tn+' '+trim(super->fio)
      sele svo
      seek vrempere->o_vo
      set color to &ecol
      @ 12,22 say 'Нет cтроки для начисления '
      set color to &stdc
      @ 13,22 say vrempere->o_vo+' '+if(found(),trim(svo->naim),'')+;
                  '.Меcяц '+vrempere->o_km+;
                  '. Сумма '+ltrim(str(vrempere->o_svon/100))
      set color to &ecol
      @ 14,22 say ' Запомните его и нажмите ┘ '
      inkey(0)
      sele vrempere
      restscreen(10,20,15,79,ScanScr)
      
return
