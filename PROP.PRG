Parameters Sum
private Bank,Sumc,Prop,Adr,i,T,J,Kc,P,K,Adr,Prop,Baza
Bank= '001���� 002��� 003�� 004���� 005���� '+;
      '006���� 007ᥬ� 008��ᥬ� 009������ 010������ '+;
      '011���������� 012��������� 013�ਭ����� '+;
      '014���ୠ���� 015��⭠���� 016��⭠���� '+;
      '017ᥬ������ 018��ᥬ������ 019����⭠���� '+;
      '020������� 030�ਤ��� 040�ப 050���줥��� '+;
      '060���줥��� 070ᥬ줥��� 080��ᥬ줥��� '+;
      '090���ﭮ�� 100�� 200����� 300���� 400������ '+;
      '500������ 600������ 700ᥬ��� 800��ᥬ��� '+;
      '900��������   '
Sumc = strzero(Sum,12,2)
if Sum=0 
   Prop = '���� ' 
 else         
   Adr = '000'
   Prop = '' 
   for i=1 to 3
      T = (I-1)*3 
      Bank = iif(I=2,stuff(Bank,4,11,'���� 002���'),stuff(Bank,4,11,'���� 002���'))
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
           Prop = Prop+'�����'
           if K>0 .and. K<=4 .and. substr(Sumc,5,1)<>'1'
              Prop = Prop+substr('����',K,1)
           endif
           Prop = Prop+' '
        case I=1 .and. Sum>=1000000
           Prop = Prop+'�������'
           if K>=2 .and. K<=4 .and. substr(Sumc,2,1)#'1' .and. substr(Sumc,3,1)#'0'
              Prop = Prop+'�'
            else
              if K>=5 .or. substr(Sumc,2,1)='1' .or. substr(Sumc,3,1)='0'
                 Prop = Prop+'��'
              endif
           endif
           Prop = Prop+' '
     endcase
  next
endif
P = asc(Prop)
P = iif(P>=160 .and. P<=175,P-32,P-80)
Prop = stuff(Prop,1,1,chr(P))
Prop = left(Prop,len(Prop)-1)+'#��. '+right(Sumc,2)+'#���.'
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
