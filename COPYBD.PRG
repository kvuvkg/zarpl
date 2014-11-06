**************************************************
************ COPYBD.PRG **************************
******* КОПИЯ БАЗЫ ДАННЫХ ************************
**************************************************
private I,KOL,I_M,S_M,YES,J,DI,TEMP,S_D,D_S,IL,SCR
store '*.DBF' to TEMP
store adir(TEMP) to KOL
declare I_M[KOL],S_M[KOL],IM[KOL]
adir(TEMP,I_M,S_M)         
store 0 to YES,DI
save screen to SCR
*setcolor(COL2)
@ 0,0 clear to 24,40
@ 0,5 say '   КОПИЯ БАЗЫ ДАННЫХ '
store 7 to J
store 0 to MI
select (0)
@ 0,34 say KOL picture '9999'
store KOL to UIU
do while YES<UIU
   store DI+1 to DI
   @ 2,3 say '    Вставте '+str(DI,2)+'-ю дискету в А:\    '
   @ 3,3 say 'и нажмите любую клавишу (Esc - конец)'
   if inkey(0)=27             
      return
   endif
   @ 3,3 say '   Удаление инфоpмации с дискеты     '
   store adir('A:\*.*') to I
   declare M_I[I]
   adir('A:\*.*',M_I)
   for IL=1 to I
       store 'A:\'+ltrim(rtrim(M_I[IL]))+' > nul ' to TEMP
       run erase &TEMP
   next
   @ 3,3 say '                                     '
   store diskspace(1) to S_D
   for I=1 to UIU 
     if at('***',I_M[I])=0 
         if S_M[I]>S_D     
             do GOL
             @ J,1 say I_M[I]+' Копия отложена      '+ltrim(str(S_M[I],8))
             store YES+1 to YES
             store MI+1 to MI
             store I_M[I] to IM[MI]
             store replicate('*',len(I_M[I])) to I_M[I]
           else
             store diskspace(1) to D_S 
             if S_M[I]<D_S       
               do GOL
               @ J,1 say I_M[I]
               store I_M[I]+' a:\ > nul' to TEMP
               @ J,14 say ' Копиpование'
               run copy &TEMP
               @ J,14 say ' Копия выполнена '+ltrim(str(S_M[I],8))
               store YES+1 to YES
               store replicate('*',len(I_M[I])) to I_M[I]         
             endif        
         endif
      endif 
  next 
enddo 
inkey(3)
@ 1,0 clear to 24,40
for JL=1 to MI
  use
  ? chr(7)
  @ 5,2 say 'Для копирования файла '+IM[JL]
  @ 6,2 say 'нажмите любую клавишу (ESC - отказ)'
  if inkey(0)=27
      loop
  endif
  @ 14,1 clear to 18,39
  if open(IM[JL])      
     @ 11,3 say '  Занят                     '
     inkey(0)
     loop
  endif
  @ 16,1 to 18,39 double
  store reccount() to COUNT
  go TOP
  @ 17,3  say replicate('░',35)
  store 21 to J
  store 1 to D 
  store 0 to I_K
  do while .T.
     ? chr(7)
     @ 22,1 say 'Вставте '+ltrim(str(D,2))+'-ю дискету в дисковoд A '
     @ 23,1 say 'и нажмите любую клавишу (Esc - конец)'
     if inkey(0)=27
        exit
     endif 
     @ 11,3 say '   Удаление инфоpмации с дискеты     '
     store adir('A:\*.*') to I
     declare M_I[I]
     adir('A:\*.*',M_I)
     for IL=1 to I
         store 'A:\'+ltrim(rtrim(M_I[IL]))+' > nul ' to TEMP
         run erase &TEMP
     next
     if D=1
         NF=fcreate('A:\COUNT.CNT')
         if ferror()=0
            NF=fopen('A:\COUNT.CNT',1)
            if ferror()=0
               fwrite(NF,str(COUNT,10)+space(10-len(alias()))+;
                        alias()+chr(26),21)
               if ferror()=0
                  fclose(NF)
               endif
             endif
          endif
      endif
      store diskspace(1) to D_S
      store 0 to K_Z
      for J=1 to 1000
          if len(fieldname(J))>1
             store K_Z+1 to K_Z
           else
             exit
           endif
       next
       store int((D_S-(32*K_Z+35))/recsize()) to K_Z
       if K_Z>0
          store 'A:\'+alias()+ltrim(str(D,2))+'.DBF' to NAME1
          @ 11,3  say NAME1+'       Идет копиpование'
          copy to &NAME1 for recno()>I_K .and. recno()<=I_K+K_Z
          store I_K+K_Z to I_K
          if I_K>=COUNT
             exit
           else
             store D+1 to D
           endif
        else
           @ 11,3  say 'Hет места на дискете поставте дpугую'
           inkey(0)
        endif
        @ 17,3  say replicate('▓',int((I_K*35)/COUNT))    
   enddo
   use
   @ 17,3 say replicate('▓',35)
   @ 11,1 say space(37)
   @ 22,1 say '   Файл скопиpован на '+ltrim(str(D,2))+' дискеты(у)'
   @ 23,1 say '        Нажмите любую клавишу         '
   inkey(0)
   @ 22,1 say '                                      '
   @ 23,1 say '                                      '
next
@ 24,1    say '   KОНЕЦ РАБОТЫ. Нажмите любую клавишу' 
inkey(0)
restore screen from SCR
return
******************************************************
procedure GOL
store J+1 to J
if J>22
   store 22 to J
   scroll(7,0,22,40,1)
endif
return
**********************************************************
function OPEN
parameters FILE
use &FILE
if .not. neterr()
   return .F.
 else
   return .T.  
endif
************************************************************
