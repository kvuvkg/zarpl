ksp1='    '
clear
@ 10,10 say '!  '
sele b
use
use superlis index superpon
sele d
use ssp
do while.not.eof()
 ksp1=ksp
 ?'                 '+ksp+'  '+nbrig          
 ?'--------------------------------------------------------'
 ?'| Ą.  |  Ŧ¨Ģ¨ī , ¨Ŧī, ŽâįĨáâĸŽ | ĒĢ ¤     | āŽæĨ­â |'
 ?'|­ŽŦĨā |                         | /áâ ĸĒ /  | ¯āĨŦ¨¨  |' 
 ?'--------------------------------------------------------'
 sele 1
 use base&ksp1
* sort to zz on tn
* use zz
 do while.not.eof()
  if tn#'******'
   tn1=tn
   ?'| '+tn+'  '
   sele b
   seek ltrim(ksp1+tn1)
   ?? superlis->fio+'   '
   sele a
   ??str(oks/100,13,2)+'    '+str(kty/100,5,2)+'   |'
  endif
  skip
 enddo ˙
 ?'--------------------------------------------------------'
 sele a
 USE
 sele d
 skip
enddo
close all
erase zz.dbf
