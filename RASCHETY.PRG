priv i,j,nu,wrk,wrk1,cur_mes,back8,n,s1,s2,ss,sb,ss1,ss2,sb1,sb2,clip,tk,fzp,fmp,fzp1,fmp1
clip=.t.
sele 0
use srd
cur_mes=val(mespod)
pred_mes=iif(cur_mes=1 ,12,cur_mes-1)
sled_mes=iif(cur_mes=12, 1,cur_mes+1)
w_sled=substr(str(100+sled_mes,3),2,2)
w_pred=substr(str(100+pred_mes,3),2,2)
sele basef
go top
do while .not. eof()
   @ 5,3 say '                                     '
   @ 5,5 say 'Расчет средних '+tn
   tk=TestKale() && if(type('basef->kalendar')<>'C','',trim(basef->kalendar))
   for i=1 to 6
      nu=str(i,1)
      nu_oper=ltrim(str(i+6,2))
      if uvu&nu=ku&nu_oper
         j=uso&nu-sku&nu_oper
         repl uso&nu with iif(j<0,0,j)
      endif
   next
   repl oz&mespod with ozb1,oh&mespod with ohb1,;
        ozb1 with ozb2, ozb2 with ozb3, ozb3 with 0,;
        ohb1 with ohb2, ohb2 with ohb3, ohb3 with 0
   for i=1 to 12
      nu=ltrim(str(i))
      wrk=kmn&nu
      wrk1=cur_mes-val(wrk)
      if vo&nu<>' '
         j=ascan(kodna,vo&nu)
         if j>0 .and. j<=len(kodna)
            m1=maskna[j]
            oplats=svon&nu
            if (wrk1>=0 .and. wrk1<9) .or. wrk1<-3
               back8=.t.
            else
               back8=.f.
               wrk2 =str(abs(if(wrk1>=9,wrk1-12,wrk1)),1)
            endif
            if substr(m1,10,1)='1'
               if back8
                  repl oz&wrk with oz&wrk+oplats
               else
                  repl ozb&wrk2 with ozb&wrk2+oplats
               endif
            endif
            if substr(m1,11,1)='1'
               if back8
                  repl oh&wrk with oh&wrk+oplats
               else
                  repl ohb&wrk2 with ohb&wrk2+oplats
               endif
            endif
         endif
      endif
   next
   store 0 to n,s1,s2
   for i=1 to 12
      nu=substr(str(100+i,3),2,2)
      if oz&nu<>0
         n=n+1
      endif
      s1=s1+oz&nu
      s2=s2+oh&nu
   next
   if n<>0
      repl ozm with s1/n, ohm with s2/n
   endif
   * midle & bolnichnye
   sele basef
     if kr='01'  
      sele srd
      go cur_mes
      d_cur=dn
*      go pred_mes
 *     d_cur=dn+d_cur
      sele basef
      rabdays = rabd&mespod
    * + rabd&w_pred
      store 0 to ss,sb
    if rabdays=0
           repl cods&w_sled with cods&mespod,;
                codb&w_sled with codb&mespod
     else
         store 0 to ss1,ss2,sb1,sb2,fzp1,fmp1,fzp2,fmp2,fzp,fmp
         do Nazad with cur_mes-1,cur_mes,mespod,ss1,sb1,fzp1,fmp1
*         do Nazad with cur_mes-1,cur_mes,w_pred,ss2,sb2,fzp2,fmp2
         ss=(ss1+ss2)*100
         sb=(sb1+sb2)*100
         fzp=fzp1+fzp2
         fmp=fmp1+fmp2
         sele basef
        if pts='2'
            ss=ss*prod_rd&tk
            sb=sb*prod_rd&tk
        endif
              repl cods&w_sled with ss/rabdays + stz,;
              codb&w_sled with sb/rabdays + stz
              repl ozm with  fzp/2+zarp13
              repl ohm with fmp/2+stz
     endif
   endif

     if kr='02'  
      sele srd
      go cur_mes
      d_cur=dn
      go pred_mes
      d_cur=dn+d_cur
      sele basef
      rabdays = rabd&mespod + rabd&w_pred
      store 0 to ss,sb
    if rabdays=0
           repl cods&w_sled with cods&mespod,;
                codb&w_sled with codb&mespod
     else
         store 0 to ss1,ss2,sb1,sb2,fzp1,fmp1,fzp2,fmp2,fzp,fmp
         do Nazad with cur_mes-1,cur_mes,mespod,ss1,sb1,fzp1,fmp1
         do Nazad with cur_mes-1,cur_mes,w_pred,ss2,sb2,fzp2,fmp2
         ss=(ss1+ss2)*100
         sb=(sb1+sb2)*100
         fzp=fzp1+fzp2
         fmp=fmp1+fmp2
         sele basef
        if pts='2'
            ss=ss*prod_rd&tk
            sb=sb*prod_rd&tk
        endif
              repl cods&w_sled with ss/rabdays + stz,;
              codb&w_sled with sb/rabdays + stz
              repl ozm with  fzp/2+zarp13
              repl ohm with fmp/2+stz
     endif
   endif
   sele srd
   go sled_mes
   sele basef
   skip
enddo
sele srd
use
set proc to
return
*
proc Nazad
para s_mes,e_mes,mes,sred,boln,f_zp,f_mp
priv i,ib,iplus
sele basef
ib=(recno()-1)*12
i=s_mes
do while i<=e_mes
 sele sebaf
 iplus=iif(i<=0,i+12,i)
 go ib+iplus
 do TwoSum with iplus,mes,sred,boln,f_zp,f_mp
 i=i+1
enddo
return
*
proc TwoSum
para i_mes,mes,sred,boln,f_zp,f_mp
priv i,nu,obl
obl=str(select())
for i=1 to 12
  nu=ltrim(str(i))
  wrk=vo&nu
  if wrk<>' ' .and. kmn&nu=mes
     j=ascan(kodna,wrk)
     if j>0 .and. j<=len(kodna)
        m1=maskna[j]
        sele &obl
        oplats=svon&nu
        if substr(m1,8,1)='1'
           sred=sred+oplats
        endif
        if substr(m1,9,1)='1'
           boln=boln+oplats
       endif
        if substr(m1,10,1)='1'
           f_zp=f_zp+oplats
       endif
        if substr(m1,11,1)='1'
           f_mp=f_mp+oplats
       endif
     endif
  endif
  sele &obl
next
return
*
