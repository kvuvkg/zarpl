**”‚€… ‘“› ’…‘’
*proc script
PARA V_ITOG,TSUM,TSUM1,TSUM2,max_len
priv i,j,r_len,t_rub
if type('max_len')<>'N'
   max_len=70
Endif

TSUM=' '
TSUM1=' '
TSUM2=' '
 
  E1=' ‹‹ ' 
  E2=' ‹‹€ '
  E3=' ‹‹‚ '
  A1='‘’'
  A2='„‚…‘’'
  A3='’‘’€'
  A4='—…’›…‘’€'
  A5='’‘’'
  A6='…‘’‘’'
  A7='‘…‘’'
  A8='‚‘…‘’'
  A9='„…‚’‘’'
  B1='„…‘’'
  B2='„‚€„–€’'
  B3='’„–€’'
  B4='‘K'
  B5='’„…‘’'
  B6='…‘’„…‘’'
  B7='‘…„…‘’'
  B8='‚‘…„…‘’'
  B9='„…‚H‘’'
  C1='„H'
  C2='„‚A'
  C3='’'
  C4='—…’›'
  C5='’'
  C6='…‘’'
  C7='‘…'
  C8='‚‘…'
  C9='„…‚’'
  D3='’'
  D4='—…’›…'
  D5='’'
  D6='…‘’'
  D7='‘…'
  D8='‚‘…'
  D9='„…‚’'
  *
  STOR 0 TO S1,S2,S3,S4,S_5
  D1='„H€'
  D2='„‚…'

IF V_ITOG>1000000
  S_5=INT(V_ITOG/1000)    
  S=INT(VAL(SUBSTR(LTRIM(STR(V_ITOG)),2,5))/100)
IF S_5>=1000.AND.S_5<=1999
S4=INT(S_5/1000)   && ¬¨««¨®­λ
S5=STR(S4,1)
TSUM=TSUM+C&S5+E1
ENDIF

IF S_5>=2000.AND.S_5<=4999
S4=INT(S_5/1000)
S5=STR(S4,1)
TSUM=TSUM+IF(S5='2',C&S5,D&S5)+E2
ENDIF

IF S_5>=5000
S4=INT(S_5/1000)
DO CASE
CASE S4>4.AND.S4<10
S5=STR(S4,1)
TSUM=TSUM+D&S5+E3
CASE S4=10
S5=STR(S4,2)
TSUM=TSUM+B1+E3
CASE S4>10.AND.S4<20
S5=SUBSTR(STR(S4,2),2,1)
TSUM=TSUM+IF(S5='2',D2,C&S5)+'€„–€’'+E3
ENDCASE
ENDIF
ELSE
  S=INT(V_ITOG/1000)  && βλαοη¨
ENDIF
  IF S>0
   S1=INT(S/100)  && α®β­¨ βλαοη
   X=MOD(S,100)
   S2=INT(X/10)   && ¤¥αοβª¨ βλαοη
   S3=MOD(X,10)  && ¥¤¨­¨ζλ βλαοη
   IF S1>0
    ST=STR(S1,1)
    TSUM=TSUM+A&ST
   ENDI
   IF S2>0
    ST=STR(S2,1)
    IF S2<>1
     TSUM=TSUM+' '+B&ST
    ENDI
   ENDI


   ST=STR(S3,1)
   DO CASE
    CASE S2=1 .AND. S3=0
     TSUM=TSUM+' '+B1
    CASE S2=1 .AND. S3>0
     TSUM=TSUM+' '+if(s3=2,'„‚…',C&ST)+'H€„–€’'
    CASE S2<>1 .AND. S3>0
     TSUM=TSUM+' '+D&ST
   ENDC

   TSUM=TSUM+' ’›‘—'
   DO CASE
    CASE S2<>1 .AND. S3=1
     TSUM=TSUM+'€'
    CASE S3>1 .AND. S3<5
     TSUM=TSUM+'' &&
CASE S3=5
TSUM=TSUM+''


   ENDC
   R_LEN=LEN(TSUM)
  ENDI     && S>0 - ¤«ο βλαοη
  *
  STOR 0 TO S1,S2,S3
  D1='„H'
  D2='„‚€'
  S=MOD(V_ITOG,1000)

  IF S>0
   S1=INT(S/100)     && α®β­¨
   X=MOD(S,100)
   S2=INT(X/10)      && ¤¥αοβª¨
   S3=MOD(X,10)     && ¥¤¨­¨ζλ

   IF S1>0
    ST=STR(S1,1)
    TSUM=TSUM+' '+A&ST
   ENDI

   IF S2>0 .AND. S2<>1
    ST=STR(S2,1)
    TSUM=TSUM+' '+B&ST
   ENDI

   ST=STR(S3,1)

   DO CASE
    CASE S2=1 .AND. S3=0
     TSUM=TSUM+' '+B1
    CASE S2=1 .AND.S3>0
     TSUM=TSUM+' '+if(s3=2,'„‚…',C&ST)+'H€„–€’'
    CASE S2<>1 .AND. S3>0
     TSUM=TSUM+' '+D&ST
    CASE S2=0 .AND. S3=1
     TSUM=TSUM+' „H'
   ENDC

  ENDI
  *TSUM=TSUM+'  “‹'
  t_rub='     ƒ‚'
  DO CASE
   CASE (S3=0 .OR. (S3>4 .AND. S3<=9)) .OR. s2=1 &&(S2=1 .AND. S3=1)
    t_rub=t_rub+'…'
   CASE S3=1
    t_rub=t_rub+'€'
   CASE S3>1 .AND. S3<5
    t_rub=t_rub+'›'
  ENDC
*T_RUB='€.'
  tsum=t_rub+' '+ltrim(trim(tsum))
  IF LEN(TSUM) > max_len   && 70
   tsum=tsum+' '
   j=1
   r_len=1
   i=min(max_len,len(tsum))
   do while .t.
      j=at(' ',substr(tsum,r_len))
      if r_len+j>=i .or. j=0
         exit
      endif
      r_len=r_len+j
   enddo
      
   TSUM1=trim(SUBS(TSUM,1,R_LEN-1))
   TSUM2=ltrim(trim(SUBS(TSUM,R_LEN)))
  ELSE
   TSUM1=TSUM
  ENDI
  * ª®­¥ζ δ®ΰ¬¨ΰ.β¥ªαβ  αγ¬¬λ *
  RETURN
