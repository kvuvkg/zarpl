SET COLOR TO &STDC
CLEAR
do while .t.
save screen to q
SET COLOR TO &bcol
@ 2,8 to 7,66 
@ 2,33 SAY ' Индексация заpаботной платы ' 
SET COLOR TO &MCOLBLN
  @  3, 10 prompt '*'
  @  4, 10 prompt '*'
  @  5, 10 prompt '*'
  @  6, 10 prompt '*'
SET COLOR TO &STDC  
     @  3, 12 say 'По одному цеху,по одному или гpуппе табельных номеpов'
     @  4, 12 say 'Возват подоходного налога                            '
     @  5, 12 say 'Подоходный налог по 13 заработной плате               '
     @  6, 12 say 'Выход в основное меню                                '
SET COLOR TO&ECOL
  @ 23,0 clear
  @ 23,1 SAY '┘-ввод F1-помощь {Esc}-выход {9}-предыдущий {3}-след.работник '
  @ 24,1 say 'F2-лицевой счет F4-долговременная F5-расчетная информация F6-Архив'
SET COLOR TO &MCOLBLN  
  PressKey=0
  set key -1 to PressCr
  set key -3 to PressCr
  set key -4 to PressCr
  set key -5 to PressCr
  set key 18 to PressCr
  set key  3 to PressCr
  set key 27 to
  set escape on
  menu to rrr
  set curs off
  set key -1 to
  set key -3 to
  set key -4 to
  set key -5 to
  set key 18 to
  set key  3 to
SET COLOR TO &STDC  
  @ 23,0 clear
  if PressKey=28
     keyboard chr(28)
  endif
  if rrr=0
     exit
  endif
     rrr=rrr+1
  do case
    case rrr=2
do in1 
restore screen from q
    case rrr=3
do in5
restore screen from q
    case rrr=4
do in2
restore screen from q
    case rrr=5
set device to screen
set console on
set print off
retu
  endcase
enddo
  set curs off
set device to screen
set console on
set print off
set confirm off
return
