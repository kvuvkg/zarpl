setcolor(stdc)
clear
setcolor(ucol)
@ 1,1 say ' Hачисление месячной премии '
sele 0
use svo index svoindex
set filter to kvoy>='401'
go top
stor '' to PremKod, PremName
do GetKodNa with PremKod, PremName
use

if len(PremKod)=0
   return
endif
befor_m=val(mespod)-1
if befor_m<1
   befor_m='12'
else
   befor_m=substr(str(100+befor_m,3),2,2)
endif
setcolor(stdc)
@ 24,0
setcolor(ucol)
@ 5,1 say ' '+PremKod+': '
@ 6,1 say ' '+trim(PremName)+' '
@ 10,1 say ' Месяц: '
setcolor(mcolbln1)
@ 10,12 prompt ' текущий ('+mespod+')'
@ 11,12 prompt ' прошлый ('+befor_m+')'
imes=1
menu to imes
if imes=0
   return
elseif imes=1
   cur_mes=mespod
else
   cur_mes=befor_m
endif
setcolor(ucol)
@ 15,1 say ' Процент: '
setcolor(mcolbln1)
@ 15,12 prompt ' общий  '
@ 16,12 prompt ' список '
iproc=1
menu to iproc
if iproc=0
   return
endif
sele basef
rele arr_prem, i_prem, allna
declare arr_prem[recc()],i_prem[recc()],allna[12]
afill(arr_prem,0)
afill(i_prem,-1)
setcolor(stdc)
sele 0
use svo index svoindex
i=recc()
declare kodna[i], maskna[i]
do ReadMskM with kodna, maskna
use
sum_prem=0
if iproc=1
   val_proc=0
   set curs on
   set curs off
   if lastkey()<>27
      do CommProc
   endif
   do ErrorTxt with ' Начислено по подразделению '+str(sum_prem,10,2)
else
   setcolor(ecol)
   @ 24,0
   @ 24,5 say '...Ждите...'
   sele basef
   set filter to ys='0'
   go top
   do while .not. eof()
      wrk=PremFnd(PremName,PremKod,cur_mes)
      sele basef
      skip
   enddo
   set filter to
   do MyDbEdit with trim(PremName)+' ('+PremKod+')',;
      kodpod,namepod,mespod,mesname,"if(i_prem[recno()]<=0,"+;
                                    "'<<<нет места в лицевом счете>>>',"+;
                                    "str(arr_prem[recno()],8,2))",;
      ' Таб.     Фамилия И. О.           Сумма  ',;
      ' номер                            премии ',;
      'MPremGet'
endif
rele arr_prem,i_prem,allna,kodna,maskna
sele basef
return
*
proc MPremGet
priv i,j,j1,i1,nu,isu,idni,oldcol
j=i_prem[recno()]
if j<=0
   return
endif
nu=ltrim(str(j,2))
oldcol=setcolor(ecol)
@ 24,0
@ 24,5 say 'Вводите'
val_proc=0
setcolor(stdc)
set curs on
@ ed_start-1+i_row,c_get+20 say ' % премии: ';
                            get val_proc pict '###.###'
read
set curs off
if lastkey()<>27
   do OneMenPr
endif
setcolor(ucol)
@ 1,40 say ' Начислено по подразделению '+str(sum_prem,10,2)
setcolor(oldcol)
return
*
proc CommProc
priv i,sum,oldcol
sele basef
set filter to ys='0'
go top
oldcol=setcolor(ucol)
@ 24,0
@ 24,5 say '...Идет расчет премии...Ждите...'
do while ! eof()
   @ 24,70 say tn
   do OneMenPr
   sele basef
   skip
enddo
sele basef
set filter to
setcolor(oldcol)
return
*
func SumPrem
priv summa,nu,m1,j,wrk
summa=0
for i=1 to 12
   nu=ltrim(str(i,2))
   wrk=vo&nu
   if wrk<>' '
      j=ascan(kodna,wrk)
      if j>0 .and. j<=len(kodna)
         m1=maskna[j]
         if substr(m1,4,1)='1'
            summa=summa+svon&nu
         endif
      endif
   endif
next
return summa
*
proc OneMenPr
priv i,sum
sele basef
if imes=2
   i=recno()
   sele sebaf
   go (i-1)*12+val(cur_mes)
endif
sum=SumPrem()
sele basef
i=i_prem[recno()]
if i=-1
   wrk=PremFnd(PremName,PremKod,mespod)
   i=i_prem[recno()]
endif
if i>0 .and. i<13
   wrk=ltrim(str(i,2))
   sum_prem=sum_prem-arr_prem[recno()]
**** ввод пpемии из кту
   repl vo&wrk with PremKod, svon&wrk with round_(sum*basef->kty/10000),;
        kmn&wrk with mespod, dn&wrk   with rabd&cur_mes
   arr_prem[recno()]=svon&wrk/100
   sum_prem=sum_prem+svon&wrk/100
*****
endif
return
*
proc Ko13zGet
priv i
@ 24,1 say 'Вводите'
setcolor(stdc)
i=knzp
@ ed_start-1+i_row,c_get get i picture '####' valid (i>=-999 .and. i<=999)
set curs on
read
set curs off
if lastkey()<>27
   repl knzp with i
endif
return
*
