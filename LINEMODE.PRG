*LineMode
*05/03/89 18:27
para imenu
    
sele basef


do while .t.
   
   set color to &mcol
   
   @ 8,2  prompt ' 1 ' message '┘ - движение по столбцам   {Esc} - конец'
   i=1
   sele srd
   do while i<13
      go i
      @ i+9,0 prompt srd->naim message '┘ - выбор   {Esc} - конец'
      i=i+1
   enddo
   
   menu to imenu
   
   set color to &stdc

   sele basef

   if imenu=0 .or. imenu=1
      exit
   endif
   
   imenu=imenu-1
   i1=substr(str(100+imenu,3),2,2)
   set curs on
   vrem01 = rabd&i1 / 100
   @ imenu+9,09 get vrem01 picture '###.##'
   vrem02 = oz&i1 / 100
   @ imenu+9,16 get vrem02 picture '####.##'
   vrem03 = oh&i1 / 100
   @ imenu+9,26 get vrem03 picture '#######.##'
   vrem04 = codb&i1 / 100
   @ imenu+9,36 get vrem04 picture '#######.##'
   vrem05 = cods&i1 / 100
   @ imenu+9,46 get vrem05 picture '#######.##'
   read
   set curs off
   repl rabd&i1 with vrem01*100,oz&i1 with vrem02*100,oh&i1 with vrem03*100,;
        codb&i1 with vrem04*100,cods&i1 with vrem05*100
   
   
   sele srd
   go imenu
   sele basef
   @ imenu+9,0 say srd->naim+str(rabd&i1/100,7,2)+str(oz&i1/100,9,2)+' '+;
                   str(oh&i1/100,9,2)+' '+str(codb&i1/100,9,2)+' '+str(cods&i1/100,9,2)
   
   imenu=imenu+1

enddo

return
*
