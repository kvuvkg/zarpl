clear
mn='  '
set cursor on
set confirm on
@ 5,5 SAY '������ ����� �� ���p� ������� �������'
@ 7,5 get mn picture '99'valid(mn>='01'.and.mn<='12')
read
setcolor(stdc)
@ 0,0 clear
do while.t.
setcolor(bcol)
@ 3,11,22,58 box frame+' '
@ 3,15 say '                           ���� �� ��ᮡ��'
setcolor(mcol)
@ 5,16  prompt '  1.��ᮡ�� ���� ���������            '
@ 7,16 prompt  '  2.��������� �� ��⠭��               '
@ 9,16 prompt  '  3.��ᮡ�� ���� p�����                '
@ 11,16 prompt '  4.��������� �� ������                '
@ 13,16 prompt '  5.��ᮡ�� ���� �୮���楢           '
@ 15,16 prompt '  6.��ᮡ�� ���� ����� ��.�����⮬   '
@ 17,16 prompt '  7.������ ���p��樨 � ����             '
@ 19,16 prompt '  8.���⪠ ���                           '
@ 21,16 prompt '  9.��室                                '              
setcolor(ecol)
@ 23,0 clear
@ 23,1 say ' �⬥��� �㦭� ���,������ �/{����} � ᮮ⢥�����饩 ��ப�'
setcolor(mcol) 
menu to z1
setcolor(stdc)
save screen to scr
do case
case z1=1
do spr WITH '1','��ᮡ�� ���� ���������','520',' �� 3-�  3 - 6  6 - 18',mn
restore screen from scr
case z1=2
do SPR WITH '2','��������� �� ��⠭��','524',' �� 3-�  3 - 6  6 - 10  10- 13  13- 18  >18',mn  
restore screen from scr
case z1=3
do spr with '3','��ᮡ�� ���� p�����','527',' �� 13   13- 18',mn 
restore screen from scr
case z1=4
do spr with '4','��������� �� ������','523',' �� 6    6 - 13  13 -18',mn
restore screen from scr
case z1=5
do spr with '5','��ᮡ�� ���� �p�����楢','525',' ',mn
restore screen from scr
case z1=6
do spr with '6','��ᮡ�� ���� ����� ��.�����⮬','526',' ',mn
restore screen from scr
case z1=7
do zapi with '1'
do zapi with '2'
do zapi with '3'
do zapi with '4'
do zapi with '5'
do zapi with '6'
restore screen from scr
case z1=8
case z1=9
clear
EXIT
endcase
enddo
