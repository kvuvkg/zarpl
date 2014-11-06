*ZarpArch 20/02/89, Архив 28.2.89,09:55 call curson/off-->set curs on/off
*17/10/89 09:42

parameters NextKod
     
SELE 0
USE SVO index svoindex

i=recc()

*index on kvoy to tempsvo
declare kodna[i], maskna[i]
do ReadMskM with kodna, maskna

sele 0
use srd

sele sebaf

first_lst=.t.
first_mes=.t.
RRR=1

itog_l=20
kop_row=19
mes_menu=val(mespod)
   
DO WHILE RRR<>0
    
   sele sebaf
   
   if first_lst
     SET COLOR TO &STDC
     CLEAR
     @ 2,0
     @ 2,1 SAY  NAMEPOD
     @ 2,30 SAY  basef->tn+' '+super->FIO
     @ 1,0
     if first_mes
        txt_mes=MesVybor()
        if txt_mes==''
           exit
        endif
        first_mes=.f.
     endif
     @ 1,18 SAY 'Архив за '+txt_mes+' '+Tek_God
     sele basef
     i=recno()
     sele sebaf
     go (i-1)*12+mes_menu
     SET COLOR TO &STDC
     @ 3,0 CLEAR
     @ 3,0,6,45 box frame+' '
     
     @ 3,13 say 'H а ч и c л е н и я'
     @ 4,1  say 'Код  Код     Наименование           Сумма'
     @ 5,1  say 'меc. нач     начиcления'
     end_shap=6
     
   endif
   
   set color to &ecol
   @ 23,0 clear
   @ 24,5 say '.....Ждите.....'
   
   if first_lst
     sr=end_shap
     for N=1 to 12
       set color to &stdc
       sr=sr+1
       nu=if(n<10,str(n,1),str(n,2))
       do SayArch with sr,nu
     next
   endif
   
   set color to &mcolbln
   @  7, 5 prompt '*'
   @  8, 5 prompt '*'
   @  9, 5 prompt '*'
   @ 10, 5 prompt '*'
   @ 11, 5 prompt '*'
   @ 12, 5 prompt '*'
   @ 13, 5 prompt '*'
   @ 14, 5 prompt '*'
   @ 15, 5 prompt '*'
   @ 16, 5 prompt '*'
   @ 17, 5 prompt '*'
   @ 18, 5 prompt '*'
   
   @ kop_row,5  prompt '*'
   @ itog_l,48  prompt '*'
   
   set color to &stdc
   @ kop_row,11 say 'Копейки прошл.меcяца'
   @ kop_row,36 say str(kpm,2)
   
   set color to &stdc
   do itogArch with itog_l,.t.,.t.

   first_lst=.f.
   
   SET COLOR TO &ECOL
   @ 23,0 clear
   @ 23,1 SAY '┘-ввод F1-помощь {Esc}-выход {9}-предыдущий {3}-следующий работник'
   @ 24,1 say 'F2-лицевой счет F3-поcт. F4-долговр. F5-раcч.информация  F7-выбор меcяца'
   set color to &mcolbln
   
   PressKey=0
   set key 27 to

   set key 18 to PressCr
   set key  3 to PressCr
   set key -1 to PressCr
   set key -2 to PressCr
   set key -3 to PressCr
   set key -4 to PressCr
   set key -6 to PressCr
   set key 28 to PressCr
   
   set escape on
   menu to rrr
   set curs off
   
   set key 18 to
   set key  3 to
   set key -1 to
   set key -2 to
   set key -3 to
   set key -4 to
   set key -6 to
   set key 28 to
   keyboard ''
   
   set color to &stdc
   @ 23,0 clear
   
   do case
   
     case RRR=0
     
       NextKod=0
       exit
     
     case PressKey=-6
       first_mes=.t.
       first_lst=.t.
       loop
     
     case PressKey=-1 .or. PressKey=-2 .or. PressKey=-3 .or. PressKey=-4
     
       NextKod=PressKey
       exit
       
     case PressKey=18 .or. PressKey=3
       set color to &stdc
       do itogArch with itog_l,.t.,.f.
       do DnUpSotr
       rrr=1
       loop
       
     case rrr=13
        
        set curs on
        @ kop_row,36 get kpm picture '##'
        read
        set curs off
        loop
        
     case rrr=14
        
        set curs on
        isu=uder/100
        @ itog_l,70 get isu picture '########.##'
        read
        set curs off
        if lastkey()<>27
           repl uder with isu*100
        endif
        loop
        
   endcase
   
   sr=rrr+end_shap
   
   nu=ltrim(STR(rrr))
  
   sele sebaf
  
   set colo to &ecol
   @ 24,0
   @ 24,3 say 'Вводите код или нажмите: F1-помощь, {Esc}-отказ'
  
   SET COLOR TO &stdc
   set key 28 to HelpKod
   
   r_h=sr
   txt_h='naim'
   
   spr_obl='svo'
   cr=6
   c_h=11
   fldn='vo'-nu
   
   svo_upd=.f.
   
   sele sebaf
   
   if PressKey=28
      keyboard chr(28)
   endif
   set curs on
   @ sr,cr GET &fldn valid(tstkdolg('sebaf',spr_obl,fldn,'kvoy',r_h,c_h,txt_h,'s'))
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
   
   sele sebaf
   set color to &stdc
   @ sr,cr say &fldn
   if lastkey()=27
      loop
   endif
   IF &fldn=' '
      @ sr,2 clear to sr,44
      repl kmn&nu with ' ', vo&nu with ' ',  svon&nu with 0
      loop
   ENDIF
  
   SET COLOR TO &ECOL
   @ 24,0
   @ 24,3 SAY 'Вводите данные: '
   
   sele sebaf
   @ 24,row() say 'код меcяца, cумму начиcленного'
   
   set color to &stdc
   stor svon&nu/100 to isu
   set curs on
   @ sr,2 get kmn&nu   picture '99';
                       valid (val(kmn&nu)>0 .and. val(kmn&nu)<13;
                              .and. .not.(' ' $ kmn&nu))
   @ sr,36 get isu     picture '##########.##'
   read
   set curs off
   if lastkey()<>27
   
      repl svon&nu with abs(isu)*100
      
   endif
   
   do SayArch with sr,nu
   
   sele sebaf
   
   rrr=rrr+1
   
ENDDO
   
set color to &stdc
do itogArch with itog_l,.t.,.f.
sele srd
use

sele svo
use
sele sebaf
CLEAR
RETURN
*
proc itogArch
para ln,repl_or_no,say_or_no
priv nac,ude,dolg,idolg,r_o_n,s_o_n

if pcount()<2
   r_o_n=.f.
else
   r_o_n=repl_or_no
endif
if pcount()<3
   s_o_n=.t.
else
   s_o_n=say_or_no
endif

sele sebaf
nac=kpm+svon1+svon2+svon3+svon4+svon5+svon6+svon7+svon8+svon9+svon10+svon11+svon12
dolg=nac-sebaf->uder
idolg=int(dolg/100)

if s_o_n
   @ ln+1,0 clear to ln+1,79

   @ ln,11 say 'Итого начислено '
   @ ln,33 say str(nac/100,12,2)
   @ ln,54 say 'Итого удержано  '
   @ ln,67 say str(uder/100,12,2)

   if dolg<0
      set color to &ucol
      @ ln+1,1 say 'Долг за работающим '+str(-dolg/100,11,2)
   else
      set color to &stdc
      @ ln+1,1 say 'Остаток '+str(dolg/100 - idolg,4,2)+;
                   '.К выдаче '+str(idolg,12,2)
   endif

endif

if r_o_n
   if dolg<0
      repl oz with 0, cd with -dolg
   else
      repl oz with dolg-idolg*100, cd with 0
   endif
endif

set color to &stdc

return
*
proc SayArch
para sr,nu
   @ sr,2  SAY kmn&nu
   @ sr,6  say vo&nu
   @ sr,36 say if(svon&nu=0,space(12),str(svon&nu/100,12,2))
   IF vo&nu<>' '
     SELE svo
     seek sebaf->vo&nu
     IF EOF()
        @ sr,11 SAY 'Kод неверен '
     ELSE
        @ sr,11 SAY NAIM
     ENDIF
   ENDI
sele sebaf
return
*
