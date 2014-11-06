clear
mn='  '
set cursor on
set confirm on
@ 5,5 SAY 'Введите месяц на котоpый начислять доплату'
@ 7,5 get mn picture '99'valid(mn>='01'.and.mn<='12')
read
setcolor(stdc)
@ 0,0 clear
do while.t.
setcolor(bcol)
@ 3,11,22,58 box frame+' '
@ 3,15 say '                           Меню по пособиям'
setcolor(mcol)
@ 5,16  prompt '  1.Пособия детям ежемесячные            '
@ 7,16 prompt  '  2.Компенсация на питание               '
@ 9,16 prompt  '  3.Пособия детям pазовые                '
@ 11,16 prompt '  4.Компенсация на одежду                '
@ 13,16 prompt '  5.Пособия детям чернобыльцев           '
@ 15,16 prompt '  6.Пособия детям больным сах.диабетом   '
@ 17,16 prompt '  7.Запись инфоpмации в базы             '
@ 19,16 prompt '  8.Чистка баз                           '
@ 21,16 prompt '  9.Выход                                '              
setcolor(ecol)
@ 23,0 clear
@ 23,1 say ' Отметьте нужные формы,набирая ┘/{ВВОД} в соответствующей строке'
setcolor(mcol) 
menu to z1
setcolor(stdc)
save screen to scr
do case
case z1=1
do spr WITH '1','Пособия детям ежемесячные','520',' до 3-х  3 - 6  6 - 18',mn
restore screen from scr
case z1=2
do SPR WITH '2','Компенсация на питание','524',' до 3-х  3 - 6  6 - 10  10- 13  13- 18  >18',mn  
restore screen from scr
case z1=3
do spr with '3','Пособия детям pазовые','527',' до 13   13- 18',mn 
restore screen from scr
case z1=4
do spr with '4','Компенсация на одежду','523',' до 6    6 - 13  13 -18',mn
restore screen from scr
case z1=5
do spr with '5','Пособия детям чеpнобыльцев','525',' ',mn
restore screen from scr
case z1=6
do spr with '6','Пособия детям больным сах.диабетом','526',' ',mn
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
