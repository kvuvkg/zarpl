*DopForma
*30/03/89 10:15   24.04.89,11:14
*if type('mespod')="C"
   sele 0
   use srd
*else
*  sele a
*  use srd
*  sele b
*  use little alias basef
*  stdc='w/,w+/br'
*  mcol='+/w,w+/r'
*endif
set color to &stdc
@ 3,0 clear
@ 3,0 say ''
text
--------------------------------------------------------
| Месяц |Кол-во|Для рас.отпуск.    |  Стоим.   1 дн    |
|       |раб.дн|  из ФЗП | из ФМП  |  больн  |  средн  |
|-------|------|---------|---------|---------|---------|
|  1    |  2   |    3    |    4    |    5    |    6    |
--------------------------------------------------------
endtext
i=1
set message to 23
do while i<13
   sele srd
   go i
   sele basef
   i1=substr(str(i+100,3),2,2)
   @ i+9,0 say srd->naim+str(rabd&i1/100,7,2)+' '+str(oz&i1/100,9,2)+' '+str(oh&i1/100,9,2)+' '+str(codb&i1/100,9,2)+' '+str(cods&i1/100,9,2)
   i=i+1
enddo

sele basef

imenu=2

do while .t.

   set color to &mcol
   
   @ 8,2  prompt ' 1 ' message '┘ - движение по строкам   {Esc} - конец'
   @ 8,10 prompt ' 2 ' message '┘ - ввод кол-вa раб.днeй  {Esc} - конец'
   @ 8,19 prompt ' 3 ' message '┘ - ввод oтпусныx из ФЗП  {Esc} - конец'
   @ 8,29 prompt ' 4 ' message '┘ - ввод oтпусныx из ФМП  {Esc} - конец'
   @ 8,39 prompt ' 5 ' message '┘ - ввод cтоим. 1 дня для расчета больн.   {Esc} - конец'
   @ 8,49 prompt ' 6 ' message '┘   ввод cтоим. 1 дня для расчета средниx  {Esc} - конец'
   
   menu to imenu
   
   set color to &stdc
   
   do case
   
      case imenu=0
         
         exit
   
      case imenu=1
         
         imenu=2
         do LineMode with  imenu

         if imenu=0
            exit
         endif
         
         imenu=2
         loop

      case imenu=2
         
         gettxt='rabd'
         pictxt='###.##'
         coltxt=09
         
      case imenu=3
      
         gettxt='oz'
         pictxt='######.##'
         coltxt=16
         
      case imenu=4
      
         gettxt='oh'
         pictxt='######.##'
         coltxt=26
         
      case imenu=5
      
         gettxt='codb'
         pictxt='######.##'
         coltxt=36
         
      case imenu=6
      
         gettxt='cods'
         pictxt='######.##'
         coltxt=46
         
   endcase
   
   sele basef
   i=1
   do while i<13
      
      i1=substr(str(100+i,3),2,2)
      vrem&i1 = &gettxt&i1 / 100
      @ i+9,coltxt get vrem&i1 picture pictxt
      i=i+1
   
   enddo
   set curs on
   read
   set curs off
   i=1
   do while i<13
      
      i1=substr(str(100+i,3),2,2)
      repl &gettxt&i1 with vrem&i1 * 100
      @ i+9,coltxt say vrem&i1 picture pictxt
      i=i+1
   
   enddo
   
enddo

sele srd
use

return
*
