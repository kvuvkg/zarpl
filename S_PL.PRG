SET COLOR TO &STDC
CLEAR

do while .t.
save screen to q
SET COLOR TO &bcol
@ 2,8 to 7,66 
@ 2,33 SAY ' �������� � ��p���⭮� ���� ' 

SET COLOR TO &MCOLBLN

  @  3, 10 prompt '*'
  @  4, 10 prompt '*'
  @  5, 10 prompt '*'
  @  6, 10 prompt '*'

SET COLOR TO &STDC  
  
     @  3, 12 say '�� ������ ���,�� ������ ��� �p㯯� ⠡����� ����p��'
     @  4, 12 say '�롮p�筮 �� �堬 �� �ᥬ ⠡���� ����p��         '
     @  5, 12 say '�� �ᥬ �堬 �� �ᥬ ⠡���� ����p��              '
     @  6, 12 say '��室 � �᭮���� ����                                '

SET COLOR TO&ECOL
  @ 23,0 clear
  @ 23,1 SAY '�-���� F1-������ {Esc}-��室 {9}-�।��騩 {3}-᫥�.ࠡ�⭨� '
  @ 24,1 say 'F2-��楢�� ��� F4-������६����� F5-���⭠� ���ଠ�� F6-��娢'
SET COLOR TO &MCOLBLN  
  PressKey=0
  set key -1 to PressCr
  set key -3 to PressCr
  set key -4 to PressCr
  set key -5 to PressCr
  set key 18 to PressCr
  set key  3 to PressCr
  set key 27 to
*  set key 28 to PressCr
  set escape on
  menu to rrr
  set curs off
  set key -1 to
  set key -3 to
  set key -4 to
  set key -5 to
  set key 18 to
  set key  3 to
*  set key 28 to
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
do S_ZPL 
restore screen from q
    case rrr=3
do S_ZPL1
restore screen from q
    case rrr=4
do S_ZPL2
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

return
*Procedure PressCr
*parameters pn,ln,vn
*PressKey=lastkey()
*clear gets
*keyboard chr(13)
*return



proc sc
?' '
?' '
?' '
?'                                            ���� '+ltrim(str(li))
?'|---------------------------------------------------------|'
?'| ���ࠧ-| ������� | �������,���,����⢮|  � � � � �   |'
?'| �������|   �����   |                     |              |'
?'|---------------------------------------------------------|'
m=8
retu

