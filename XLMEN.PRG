PARA Q,W,E,R,T
clear
setcolor(stdc)
@ 0,0 clear
do while.t.
setcolor(bcol)
@ 7,11,16,58 box frame+' '
@ 7,15 say '���� ��䨪�樨'
setcolor(mcol)
@ 8,16  prompt '  1.���� �ᮢ,���,% �६�� � % ���६�� '
@ 10,16  prompt '  2.�����                               '
@ 12,16 prompt '  3.��室                                '              
setcolor(ecol)
@ 23,0 clear
@ 23,1 say ' �⬥��� �㦭� ���,������ �/{����} � ᮮ⢥�����饩 ��ப�'
setcolor(mcol) 
menu to z1
setcolor(stdc)
save screen to scr
do case
case z1=1
do PROBA WITH Q,W,E,R,T
restore screen from scr
case z1=2
do PROBA1 WITH Q,W,E,R,T
restore screen from scr
case z1=3
clear
EXIT
endcase
enddo
