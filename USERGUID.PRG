priv i,j
set color to &stdc
clear
@ 0,0 say ''
i=1
do while .t.
   do typefile with 'osnh.txt'
   set color to &mcol
   @ 4,26 prompt '"меню" и сообщения'
   @ 7,40 prompt 'постоянной'
   @ 7,52 prompt 'долговременной'
   @ 7,68 prompt 'раcчетной'
   @ 12,7 prompt 'деятельноcти'
   @ 18,8 prompt 'технологические функции'
   @ 19,8 prompt 'корректировка справочников'
   @ 20,8 prompt 'обработка информации'
   @ 21,8 prompt 'установка пароля'
   i=menuhelp(i)
   if i=0
      exit
   endif
   do case
    case i=1
      do typefile with 'dial.txt'
    case i=2
      do typefile with 'postinf.txt'
    case i=3
      do typefile with 'dolginf.txt'
    case i=4
      do typefile with 'rascinf.txt'
    case i=5
      do typefile with 'func.txt'
    case i=6
      do typefile with 'tekh.txt'
    case i=7
      do typefile with 'korr.txt'
    case i=8
      j=1
      do while .t.
         set color to &stdc
         clear
         @ 0,0 say ''
         do typefile with 'obri.txt'
         set color to &mcol
         @  5,27 prompt 'ведомоcтей'
         @ 12,62 prompt 'удержаний'
         @ 17,34 prompt 'постоянной'
         @ 17,53 prompt 'долговременной'
         @ 18,12 prompt 'раcчетной'
         @ 21,22 prompt 'повременной оплаты, больничных, отпускных'
         j=menuhelp(j)
         if j=0
            exit
         endif
         do case
            case j=1
              do typefile with 'vedo.txt'
            case j=2
              do TypeFile with 'uder.txt'
            case j=3
              do typefile with 'postinf.txt'
            case j=4
              do TypeFile with 'dolginf.txt'
            case j=5
              do TypeFile with 'rascinf.txt'
            case j=6
              do TypeFile with 'nach.txt'
         endcase
         set color to &ecol
         @ 24,0
         @ 24,4 say 'Для продолжения нажмите клавишу  ┘'
         keyboard ''
         inkey(0)
      enddo
    case i=9
      do TypeFile with 'pass.txt'
   endcase
   if i<>8
      set color to &ecol
      @ 24,0
      @ 24,4 say 'Для продолжения нажмите клавишу  ┘'
      keyboard ''
      inkey(0)
   endif
enddo
return
*
proc TypeFile
para filen
set color to &stdc
clear
@ 0,0 say ''
if file(filen)
   type &filen
else
   set color to &ecol
   @ 0,0 say 'Текст не найден'
   set color to &stdc
endif
return
*
func MenuHelp
para sj
priv j
if type('sj')<>'N'
   j=1
else
   j=sj
endif
set color to &ecol
@ 24,0
@ 24,1 say '<клавиши-cтрелки> -- движение   ┘ -- выбор   {Esc} или {Ключ} -- отказ '
set color to &mcol
menu to j
set color to &stdc
return j
