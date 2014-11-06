Parameters Sum
private Bank,Sumc,Prop,Adr,i,T,J,Kc,P,K,Adr,Prop,Baza
Bank= '001одна 002две 003три 004четыре 005пять '+;
      '006шесть 007семь 008восемь 009девять 010десять '+;
      '011одиннадцать 012двенадцать 013тринадцать '+;
      '014четырнадцать 015пятнадцать 016шестнадцать '+;
      '017семнадцать 018восемнадцать 019девятнадцать '+;
      '020двадцать 030тридцать 040сорок 050пятьдесят '+;
      '060шестьдесят 070семьдесят 080восемьдесят '+;
      '090девяносто 100сто 200двести 300триста 400четыреста '+;
      '500пятьсот 600шестьсот 700семьсот 800восемьсот '+;
      '900девятьсот   '
Sumc = strzero(Sum,12,2)
if Sum=0 
   Prop = 'ноль ' 
 else         
   Adr = '000'
   Prop = '' 
   for i=1 to 3
      T = (I-1)*3 
      Bank = iif(I=2,stuff(Bank,4,11,'одна 002две'),stuff(Bank,4,11,'один 002два'))
      for J=1 to 3
        P = T+J 
        Kc = substr(Sumc,P,1)
        K =  val(KC) 
        Adr = stuff(Adr,J,1,Kc)
        if (Adr<>'010' .or. J<>2) .and. Adr<>'000'
            P = at(Adr,Bank)
            P = P+3 
            Baza = substr(Bank,P,13)
            P = at(' ',Baza)
            Prop = Prop+substr(Baza,1,P)
            Adr =  '000'
        endif
     next
     do case
        case I=2 .and. Sum>=1000  .and. substr(Sumc,4,3)#'000'
           Prop = Prop+'тысяч'
           if K>0 .and. K<=4 .and. substr(Sumc,5,1)<>'1'
              Prop = Prop+substr('аиии',K,1)
           endif
           Prop = Prop+' '
        case I=1 .and. Sum>=1000000
           Prop = Prop+'миллион'
           if K>=2 .and. K<=4 .and. substr(Sumc,2,1)#'1' .and. substr(Sumc,3,1)#'0'
              Prop = Prop+'а'
            else
              if K>=5 .or. substr(Sumc,2,1)='1' .or. substr(Sumc,3,1)='0'
                 Prop = Prop+'ов'
              endif
           endif
           Prop = Prop+' '
     endcase
  next
endif
P = asc(Prop)
P = iif(P>=160 .and. P<=175,P-32,P-80)
Prop = stuff(Prop,1,1,chr(P))
Prop = left(Prop,len(Prop)-1)+'#грв. '+right(Sumc,2)+'#коп.'
*
Bank = ''
Adr  = ''
I    = 1
do while ( I<>0 )
   I   = at(' ',Prop)
   Adr = left(Prop,I)
   if len(Adr+Bank) < 51
      Bank = Bank + Adr
    else
      ? strtran(Bank,'#',' ')
        Bank = Adr
   endif
   Prop = substr(Prop,I+1)
enddo
Bank = Bank + Prop
? strtran(Bank,'#',' ')
?' '
return 
