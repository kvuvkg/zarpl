*** 19.06.92
*** Работа с аpхивом индексация
*** in1.prg
set device to screen
priv mes,mes1,k,s,m,li,tn1,tn2,kk,vv,v_v,v_v1,v_v2,k,gt,gb,i,f_io,vv3,vv4,v_v3,v_v4,M_M,us,un,ur
PRIV S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_BL,S_ZP,RAZN,s_b,S_IN,x1,x2
store 0 to z,z1,z2,z3,z4,us,un,ur,x1,x2
M_M='  '
sele f
use ssp
@ 0,0 CLEAR
STORE 0 TO S_1,S_2,S_3,S_4,S_5,S_6,S_7,S_BL,S_ZP,RAZN,s_b,S_IN
STORE 0 TO K,s,m,li
li=1
Per='           '
Tnn='             '
store '  ' to mes,mes1
STORE '      ' TO TN1,TN2,gt,gb
store '    ' to kk
STORE '                                                    ' TO VV1,vv2,tt,f_io,vv3,vv4
store space(60) to vv,v_v,v_v1,v_v2,v_v3,v_v4
k=1
set cursor on
set confirm on
set color to &bcol
@ 1,1 to 22,70
@ 1,1 say 'Пересчет подоходного налога по одному или гpуппе табельных номеpов'
set color to &stdc
@ 3,5 SAY 'Введите номер подразделения для котоpого сделать расчет '
@ 5,5 get kk picture '9999' 
@ 23,0 clear
@ 23,1 say ' -ввод F1-помощь {Esc}-выход '
@ 24,1 say ' '
read
if lastkey()=27
retu
endif
@ 5,20 say 'Подpазделение'
sele f
locate for ksp=kk
if found()
@ 5,34 say ssp->nbrig
endif
sele a
use base&kk
go top
gt=tn
go bottom
gb=tn
go top

@ 7,5 say 'Введите месяц за котоpый нужно возвратить налог'
@ 9,5 get per picture '99.99-99.99'
@ 11,5 SAY 'Введите табельный номеp или гpуппу табельных номеpов'
@ 12,5 say 'Имеющиеся табельные номеpа находятся в пpеделах ['+gt+'-'+gb+']'
@ 15,5 GET TNN PICTURE '999999-999999'
  @ 23,0 clear
  @ 23,1 SAY '┘-ввод F1-помощь {Esc}-выход '
  @ 24,1 say ''
read
@ 23,0 CLEAR
@ 23,1 SAY 'Ж Д И Т Е!'
set cursor on
if lastkey()=27
retu
endif
set console on
TN1=SUBSTR(TNN,1,6)
TN2=SUBSTR(TNN,8,6)
IF TN2=' '
TN2=TN1
ENDIF
mes=val(substr(per,1,2))  && месяц индексации
mes1=val(substr(per,7,2))
sele a
use base&kk
set filter to tn>=tn1.and.tn<=tn2
GO TOP
if mes<10
m_m='0'+ltrim(str(mes))
else
m_m=ltrim(str(mes))
endif
do while.not.eof()
i=recno()

sele b
use seba&kk
*go (i-1)*12+mes  && если необходимо с опеделенного месяца а не с начала базы
go (i-1)*12+1
K=13
i1=1
DO WHILE K-i1#0
*DO WHILE K-mes-1#0
do fivesum1 with m_m,z,z1,z2,z3,z4
SKIP
*MES=MES+1
i1=i1+1
ENDDO
u=0
x1=z
do upd1 with m_m,z,z1,u
us=u
u=0

sele a
do fivesum2 with m_m,z,z1,z2,z3,z4
x2=z
do upd2 with m_m,z,z1,u
un=u
ur=us-un
u=0
z=0

if ur#0
if x2>x1.or.x2<x1
for i=1 to 12
   nu=if(i<10,str(i,1),str(i,2))
if ku&nu='910'.and.kmu&nu=m_m
replace ku&nu with '910',kmu&nu with m_m,sku&nu with ur
ur=0
us=0
un=0
exit
endif
next 
else
for i=1 to 12
   nu=if(i<10,str(i,1),str(i,2))
if ku&nu='   '
replace ku&nu with '910',kmu&nu with m_m,sku&nu with ur*(-1)
ur=0
us=0
un=0
exit
endif
next 
endif
endif
x2=0
x1=0
skip
enddo
SET CONFIRM OFF
return

proc FiveSum1
para mes,s1,s2,s3,s4,s5
priv nu,i,j,m1,kod,oplats
oplats=0
for i=1 to 12
   nu=if(i<10,str(i,1),str(i,2))
   if mes=kmn&nu .and. vo&nu<>' '
kod=vo&nu
oplats=svon&nu
sele h
use svo index svoindex
seek kod
if substr(zmo,5,1)='1'
            s1=s1+oplats
      endif
   endif
sele b
next
return

proc FiveSum2
para mes,s1,s2,s3,s4,s5
priv nu,i,j,m1,kod,oplats
oplats=0
for i=1 to 12
   nu=if(i<10,str(i,1),str(i,2))
   if mes=kmn&nu .and. vo&nu<>' '
kod=vo&nu
oplats=svon&nu
sele h
use svo index svoindex
seek kod
if substr(zmo,5,1)='1'
            s1=s1+oplats
      endif
   endif
sele a
next
return

proc Upd1
SET TALK OFF
PARA ras_mes,Sym1,Sym2,U
priv s1,s2,t1,t2,a,k,u1,u2
stor 0 to u1,u2,k
s1=Sym1
s2=Sym2
sele a
do case
case ras_mes>='01'.and.ras_mes<='11'
if np='7'
U=0

ELSE

do case
case np='0'.or.np='1'
k=0
case np='2'.or.np='3'
k=30/100
case np='4'.or.np='5'
k=50/100
case np='6'
k=80/100
endcase

 A=INT((S1+S2)/100)
 S1=INT(S1/100)
 S2=INT(S2/100)

 SELE a

 DO CASE

  CASE RPV='2'   && совместители
 
        DO CASE                 
        CASE A=0               
        U=0                   
        CASE A<101             
        U=A*0.02              
        CASE A>100.AND.A<201     
        U=2.0+0.03*(A-100)     
        CASE A>200.AND.A<301    
        U=5.0+0.05*(A-200)     
        case A>300.AND.A<501   
        U=10.0+0.06*(A-300)    
        CASE A>500.AND.A<1301   
        U=22.0+0.09*(A-500)   
        CASE A>1300.AND.A<2401  
        U=94.0+0.10*(A-1300)   
        CASE A>2400.AND.A<4801 
        U=204.0+0.11*(A-2400)  
        CASE A>4800.AND.A<7501 
        U=468.0+0.12*(A-4800) 
        CASE A>7500.AND.A<9901 
        U=792.0+0.15*(A-7500) 
        CASE A>9900.AND.A<14901 
        U=1152.0+0.2*(A-9900)  
        CASE A>14901            
        U=2152.0+0.3*(A-14900)  

   ENDC


  CASE  A<=900                        
  U=0                                
  CASE S1>900.AND.S1<7501             
  U=0.12*(S1-900)                    
  CASE S1>7500.AND.S1<9901            
  U=792.0+0.15*(S1-7500)             
  CASE S1>9900.AND.S1<14901            
  U=1152.0+0.20*(S1-9900)             
  CASE S1>14900                       
  U=2152.0+0.3*(S1-14900)              
                                                                                  
                                                                                  
  CASE S1>7500.AND.S1<9901            
  U=792.0+0.15*(S1-7500)             
  CASE S1>9900.AND.S1<14901           
  U=1152.0+0.20*(S1-9900)             
  CASE S1>14900                       
  U=2152.0+0.3*(S1-14900)              
                                                                                  
                                                                                  

        DO CASE                    
        CASE S2=0                 
         U2=0                     
        CASE S2<101                
         U2=S2*0.02               
        CASE S2>100.AND.S2<201      
         U2=0.2+0.03*(S2-100)      
          OTHERWISE                  
                                                                             
       DO CASE                   
        CASE S2>200.AND.S2<301     
         U2=5.0+0.05*(S2-200)      
        case S2>300.AND.S2<501    
         U2=10.0+0.06*(S2-300)     
        CASE S2>500.AND.S2<1301    
         U2=22.0+0.09*(S2-500)    
        CASE S2>1300.AND.S2<2401   
         U2=94.0+0.10*(S2-1300)    
        CASE S2>2400.AND.S2<4801  
         U2=204.0+0.11*(S2-2400)   
        CASE S2>4800.AND.S2<7501  
         U2=468.0+0.12*(S2-4800)  
        CASE S2>7500.AND.S2<9901  
         U2=792.0+0.15*(S2-7500)  
        CASE S2>9900.AND.S2<14901  
         U2=1152.0+0.2*(S2-9900)   
        CASE S2>14900              
         U2=2152.0+0.3*(S2-14900)   
                                                                             
   ENDC
   ENDC

U=U1+U2

OTHERWISE

     DO CASE

    CASE A>900.AND.A<7501          
     U=0.12*(A-900)                
    CASE A>7500.AND.A<9901         
     U=792.0+0.15*(A-7500)         
    CASE A>9900.AND.A<14901         
     U=1152.0+0.20*(A-9900)         
    CASE A>14900                    
     U=2152.0+0.3*(A-14900)          
                                                                            
     DO CASE                  
     CASE S2>200.AND.S2<301    
      U2=5.0+0.05*(S2-200)     
     case S2>300.AND.S2<501   
      U2=10.0+0.06*(S2-300)    
     CASE S2>500.AND.S2<1301   
      U2=22.0+0.09*(S2-500)   
     CASE S2>1300.AND.S2<2401  
      U2=94.0+0.10*(S2-1300)   
     CASE S2>2400.AND.S2<4801 
      U2=204.0+0.11*(S2-2400)  
     CASE S2>4800.AND.S2<7501 
      U2=468.0+0.12*(S2-4800) 
     CASE S2>7500.AND.S2<9901 
      U2=792.0+0.15*(S2-7500) 
     CASE S2>9900.AND.S2<14901 
      U2=1152.0+0.2*(S2-9900)  
     CASE S2>14900             
      U2=2152.0+0.3*(S2-14900)  
    endc
U=MIN(U1,U2)
ENDC
endc
 U=round(U*(1-K)*100,0)
ENDI
sele a
endcase
RETU


proc Upd2
SET TALK OFF
PARA ras_mes,Sym1,Sym2,U
priv s1,s2,t1,t2,a,k,u1,u2
stor 0 to u1,u2,k
s1=Sym1
s2=Sym2

sele a

do case
case ras_mes>'01'.and.ras_mes<='06'
if np='7'
U=0

ELSE

do case
case np='0'.or.np='1'
k=0
case np='2'.or.np='3'
k=30/100
case np='4'.or.np='5'
k=50/100
case np='6'
k=80/100
endcase
 A=INT((S1+S2)/100)
 S1=INT(S1/100)
 S2=INT(S2/100)

 SELE a

 DO CASE

  CASE RPV='2'   && совместители
 
        DO CASE                 
        CASE A=0               
        U=0                   
        CASE A<101             
        U=A*0.02              
        CASE A>100.AND.A<201     
        U=2.0+0.03*(A-100)     
        CASE A>200.AND.A<301    
        U=5.0+0.05*(A-200)     
        case A>300.AND.A<501   
        U=10.0+0.06*(A-300)    
        CASE A>500.AND.A<1301   
        U=22.0+0.09*(A-500)   
        CASE A>1300.AND.A<2401  
        U=94.0+0.10*(A-1300)   
        CASE A>2400.AND.A<4801 
        U=204.0+0.11*(A-2400)  
        CASE A>4800.AND.A<7501 
        U=468.0+0.12*(A-4800) 
        CASE A>7500.AND.A<9901 
        U=792.0+0.15*(A-7500) 
        CASE A>9900.AND.A<14901 
        U=1152.0+0.2*(A-9900)  
        CASE A>14901            
        U=2152.0+0.3*(A-14900)  

   ENDC


  CASE  A<=900                        
  U=0                                
  CASE S1>900.AND.S1<7501             
  U=0.12*(S1-900)                    
  CASE S1>7500.AND.S1<9901            
  U=792.0+0.15*(S1-7500)             
  CASE S1>9900.AND.S1<14901            
  U=1152.0+0.20*(S1-9900)             
  CASE S1>14900                       
  U=2152.0+0.3*(S1-14900)              
                                                                                  
                                                                                  
  CASE S1>7500.AND.S1<9901            
  U=792.0+0.15*(S1-7500)             
  CASE S1>9900.AND.S1<14901           
  U=1152.0+0.20*(S1-9900)             
  CASE S1>14900                       
  U=2152.0+0.3*(S1-14900)              
                                                                                  
                                                                                  

        DO CASE                    
        CASE S2=0                 
         U2=0                     
        CASE S2<101                
         U2=S2*0.02               
        CASE S2>100.AND.S2<201      
         U2=0.2+0.03*(S2-100)      
          OTHERWISE                  
                                                                             
       DO CASE                   
        CASE S2>200.AND.S2<301     
         U2=5.0+0.05*(S2-200)      
        case S2>300.AND.S2<501    
         U2=10.0+0.06*(S2-300)     
        CASE S2>500.AND.S2<1301    
         U2=22.0+0.09*(S2-500)    
        CASE S2>1300.AND.S2<2401   
         U2=94.0+0.10*(S2-1300)    
        CASE S2>2400.AND.S2<4801  
         U2=204.0+0.11*(S2-2400)   
        CASE S2>4800.AND.S2<7501  
         U2=468.0+0.12*(S2-4800)  
        CASE S2>7500.AND.S2<9901  
         U2=792.0+0.15*(S2-7500)  
        CASE S2>9900.AND.S2<14901  
         U2=1152.0+0.2*(S2-9900)   
        CASE S2>14900              
         U2=2152.0+0.3*(S2-14900)   
                                                                             
   ENDC
   ENDC

U=U1+U2

OTHERWISE

     DO CASE

    CASE A>900.AND.A<7501          
     U=0.12*(A-900)                
    CASE A>7500.AND.A<9901         
     U=792.0+0.15*(A-7500)         
    CASE A>9900.AND.A<14901         
     U=1152.0+0.20*(A-9900)         
    CASE A>14900                    
     U=2152.0+0.3*(A-14900)          
                                                                            
     DO CASE                  
     CASE S2>200.AND.S2<301    
      U2=5.0+0.05*(S2-200)     
     case S2>300.AND.S2<501   
      U2=10.0+0.06*(S2-300)    
     CASE S2>500.AND.S2<1301   
      U2=22.0+0.09*(S2-500)   
     CASE S2>1300.AND.S2<2401  
      U2=94.0+0.10*(S2-1300)   
     CASE S2>2400.AND.S2<4801 
      U2=204.0+0.11*(S2-2400)  
     CASE S2>4800.AND.S2<7501 
      U2=468.0+0.12*(S2-4800) 
     CASE S2>7500.AND.S2<9901 
      U2=792.0+0.15*(S2-7500) 
     CASE S2>9900.AND.S2<14901 
      U2=1152.0+0.2*(S2-9900)  
     CASE S2>14900             
      U2=2152.0+0.3*(S2-14900)  
    endc
U=MIN(U1,U2)
ENDC
endc
 U=round(U*(1-K)*100,0)
ENDI

sele a

case ras_mes='08'
sele a

if np='7'
U=0

ELSE

do case
case np='0'.or.np='1'
k=0
case np='2'.or.np='3'
k=30/100
case np='4'.or.np='5'
k=50/100
case np='6'
k=80/100
endcase
 A=INT((S1+S2)/100)
 S1=INT(S1/100)
 S2=INT(S2/100)

 SELE a

 DO CASE

  CASE RPV='2'   && совместители
 
        DO CASE                 
        CASE A=0               
        U=0                   
        CASE A<101             
        U=A*0.02              
        CASE A>100.AND.A<311     
        U=2.0+0.03*(A-100)     
        case A>310.AND.A<464   
        U=7.70+0.05*(A-310)    
        CASE A>463.AND.A<773   
        U=15.35+0.06*(A-463)   
        CASE A>772.AND.A<2007  
        U=33.89+0.09*(A-772)   
  CASE A>2007.AND.A<11581             
  U=0.12*(A-2007)                    

   ENDC

  CASE  A<=1390                        
  U=0                                
  CASE S1>1390.AND.S1<11581             
  U=0.12*(S1-1390)                    
  CASE S1>11580.AND.S1<15286            
  U=1222.92+0.15*(S1-11580)             

        DO CASE                    
        CASE S2=0                 
         U2=0                     
        CASE S2<101                
         U2=S2*0.02               
        CASE S2>100.AND.S2<311      
         U2=0.2+0.03*(S2-100)      
          OTHERWISE                  
                                                                             
       DO CASE                   
        case S2>310.AND.S2<464   
        U2=7.70+0.05*(S2-310)    
        CASE S2>463.AND.S2<773   
        U2=15.35+0.06*(S2-463)   
        CASE S2>772.AND.S2<2007  
        U2=33.89+0.09*(S2-772)   
  CASE S2>2007.AND.S2<11581             
  U2=0.12*(S2-2007)                    

                                                                           
   ENDC
   ENDC

U=U1+U2

OTHERWISE

     DO CASE

    CASE A>1390.AND.A<11581          
     U=0.12*(A-1390)                
    CASE A>11580.AND.A<15286         
     U=1222.92+0.15*(A-11580)         
                                                                            
     DO CASE                  
        case S2>310.AND.S2<464   
        U2=7.70+0.05*(S2-310)    
        CASE S2>463.AND.S2<773   
        U2=15.35+0.06*(S2-463)   
        CASE S2>772.AND.S2<2007  
        U2=33.89+0.09*(S2-772)   
  CASE S2>2007.AND.S2<11581             
  U2=0.12*(S2-2007)                    

    endc
U=MIN(U1,U2)
ENDC
endc
 U=round(U*(1-K)*100,0)
ENDI
sele a

case ras_mes='09'

sele a

if np='7'
U=0

ELSE

do case
case np='0'.or.np='1'
k=0
case np='2'.or.np='3'
k=30/100
case np='4'.or.np='5'
k=50/100
case np='6'
k=80/100
endcase
 A=INT((S1+S2)/100)
 S1=INT(S1/100)
 S2=INT(S2/100)

 SELE a

 DO CASE

  CASE RPV='2'   && совместители
 
        DO CASE                 
        CASE A=0               
        U=0                   
        CASE A<101             
        U=A*0.02              
        CASE A>100.AND.A<335     
        U=2.0+0.03*(A-100)     
        CASE A>334.AND.A<502    
        U=8.35+0.05*(A-334)     
        case A>501.AND.A<837   
        U=16.70+0.06*(A-501)    
        CASE A>836.AND.A<2174   
        U=36.80+0.09*(A-836)   
  CASE A>2175.AND.A<12541             
  U=0.12*(A-1505)                    
  CASE A>12540.AND.A<16553            
  U=1324.32+0.15*(A-12540)             
  CASE A>16552.AND.A<24913           
  U=1926.12+0.20*(A-16552)             


   ENDC


  CASE  A<=1505                        
  U=0                                
  CASE S1>1505.AND.S1<12541             
  U=0.12*(S1-1505)                    
  CASE S1>12540.AND.S1<16553            
  U=1324.32+0.15*(S1-12540)             
  CASE S1>16552.AND.S1<24913           
  U=1926.12+0.20*(S1-16552)             

        DO CASE                    
        CASE S2=0                 
         U2=0                     
        CASE S2<101                
         U2=S2*0.02               
        CASE S2>100.AND.S2<335      
         U2=0.2+0.03*(S2-100)      
          OTHERWISE                  
                                                                             
       DO CASE                   
        CASE S2>334.AND.S2<502    
        U2=8.35+0.05*(S2-334)     
        case S2>501.AND.S2<837   
        U2=16.70+0.06*(S2-501)    
        CASE S2>836.AND.S2<2174   
        U2=36.80+0.09*(S2-836)   
  CASE S2>2175.AND.S2<12541             
  U2=0.12*(S2-1505)                    
  CASE S2>12540.AND.S2<16553            
  U2=1324.32+0.15*(S2-12540)             
  CASE S2>16552.AND.S2<24913           
  U2=1926.12+0.20*(S2-16552)             


   ENDC
   ENDC

U=U1+U2

OTHERWISE

     DO CASE

    CASE A>1505.AND.A<12541          
     U=0.12*(A-1505)                
    CASE A>12540.AND.A<16553         
     U=1324.32+0.15*(A-12540)         
    CASE A>16552.AND.A<24913         
     U=1926.12+0.20*(A-16552)         
                                                                            
     DO CASE                  
        CASE S2>334.AND.S2<502    
        U2=8.35+0.05*(S2-334)     
        case S2>501.AND.S2<837   
        U2=16.70+0.06*(S2-501)    
        CASE S2>836.AND.S2<2174   
        U2=36.80+0.09*(S2-836)   
  CASE S2>2175.AND.S2<12541             
  U2=0.12*(S2-1505)                    
  CASE S2>12540.AND.S2<16553            
  U2=1324.32+0.15*(S2-12540)             
  CASE S2>16552.AND.S2<24913           
  U2=1926.12+0.20*(S2-16552)             

    endc
U=MIN(U1,U2)
ENDC
endc
 U=round(U*(1-K)*100,0)
ENDI
sele a

case ras_mes='07'
sele a
if np='7'
U=0

ELSE

do case
case np='0'.or.np='1'
k=0
case np='2'.or.np='3'
k=30/100
case np='4'.or.np='5'
k=50/100
case np='6'
k=80/100
endcase
 A=INT((S1+S2)/100)
 S1=INT(S1/100)
 S2=INT(S2/100)

 SELE a

 DO CASE

  CASE RPV='2'   && совместители
 
        DO CASE                 
        CASE A=0               
        U=0                   
        CASE A<101             
        U=A*0.02              
        CASE A>100.AND.A<251     
        U=2.0+0.03*(A-100)     
        CASE A>251.AND.A<377    
        U=6.28+0.05*(A-251)     
        case A>376.AND.A<629   
        U=15.35+0.06*(A-376)    
        CASE A>628.AND.A<1633   
        U=27.65+0.09*(A-628)   
  CASE A>1632.AND.A<9421             
  U=0.12*(A-1632)                    
  CASE A>9420.AND.A<12435            
  U=994.8+0.15*(A-9420)             
   ENDC


  CASE  A<=1130                        
  U=0                                
  CASE S1>1130.AND.S1<9421             
  U=0.12*(S1-1130)                    
  CASE S1>9420.AND.S1<12435            
  U=994.8+0.15*(S1-9420)             

        DO CASE                    
        CASE S2=0                 
         U2=0                     
        CASE S2<101                
         U2=S2*0.02               
        CASE S2>100.AND.S2<251      
         U2=2+0.03*(S2-100)      
          OTHERWISE                  
                                                                             
       DO CASE                   
        CASE S2>251.AND.S2<377    
        U2=6.28+0.05*(S2-251)     
        case S2>376.AND.S2<629   
        U2=15.35+0.06*(S2-376)    
        CASE S2>628.AND.S2<1633   
        U2=27.65+0.09*(S2-628)   
  CASE S2>1632.AND.S2<9421             
  U2=0.12*(S2-1632)                    
  CASE S2>9420.AND.S2<12435            
  U2=994.8+0.15*(S2-9420)             
                                                                             
   ENDC
   ENDC

U=U1+U2

OTHERWISE

     DO CASE

    CASE A>1130.AND.A<9421          
     U=0.12*(A-1130)                
    CASE A>9420.AND.A<12435         
     U=994.8+0.15*(A-9420)         
                                                                            
     DO CASE                  
        CASE S2>251.AND.S2<377    
        U2=6.28+0.05*(S2-251)     
        case S2>376.AND.S2<629   
        U2=15.35+0.06*(S2-376)    
        CASE S2>628.AND.S2<1633   
        U2=27.65+0.09*(S2-628)   
  CASE S2>1632.AND.S2<9421             
  U2=0.12*(S2-1632)                    
  CASE S2>9420.AND.S2<12435            
  U2=994.8+0.15*(S2-9420)             
    endc
U=MIN(U1,U2)
ENDC
endc
 U=round(U*(1-K)*100,0)
ENDI
sele a

case ras_mes='10'

sele a
if np='7'
U=0

ELSE

do case
case np='0'.or.np='1'
k=0
case np='2'.or.np='3'
k=30/100
case np='4'.or.np='5'
k=50/100
case np='6'
k=80/100
endcase
 A=INT((S1+S2)/100)
 S1=INT(S1/100)
 S2=INT(S2/100)
 SELE a
   DO CASE

  CASE RPV='2'   && совместители
 
        DO CASE                 
        CASE A=0               
        U=0                   
        CASE A<101             
        U=A*0.02              
        CASE A>100.AND.A<370     
        U=2.0+0.03*(A-100)     
        CASE A>369.AND.A<555    
        U=9.23+0.05*(A-369)     
        case A>554.AND.A<925   
        U=18.48+0.06*(A-554)    
        CASE A>924.AND.A<2403   
        U=40.68+0.09*(A-924)   
        CASE A>2402.AND.A<4309  
        U=173.7+0.10*(A-2402)   
  CASE A>9408.AND.A<11809           
  U=929.4+0.15*(A-9408)             
  CASE A>11808.AND.A<16809            
  U=1289.4+0.20*(A-11808)             
  CASE A>16808                       
  U=2289.0+0.3*(A-16808)              

   ENDC

  CASE  A<=1663                       
  U=0                                
  CASE S1>1663.AND.S1<9409             
  U=0.12*(S1-1663)                    
  CASE S1>9408.AND.S1<11809           
  U=929.4+0.15*(S1-9408)             
  CASE S1>11808.AND.S1<16809            
  U=1289.4+0.20*(S1-11808)             
  CASE S1>16808                       
  U=2289.0+0.3*(S1-16808)              
  CASE S1>9408.AND.S1<11809           
  U=929.4+0.15*(S1-9408)             
  CASE S1>11808.AND.S1<16809            
  U=1289.4+0.20*(S1-11808)             
  CASE S1>16808                       
  U=2289.0+0.3*(S1-16808)              
                                                                                  
                                                                                  

        DO CASE                    
        CASE S2=0                 
         U2=0                     
        CASE S2<101                
         U2=S2*0.02               
        CASE S2>100.AND.S2<370      
         U2=0.2+0.03*(S2-100)      
          OTHERWISE                  
                                                                             
       DO CASE                   
        CASE S2>369.AND.S2<555     
         U2=9.23+0.05*(S2-369)      
        case S2>554.AND.S2<925    
         U2=18.48+0.06*(S2-554)     
        CASE S2>924.AND.S2<2403    
         U2=40.68+0.09*(S2-924)    
        CASE S2>2402.AND.S2<4309   
         U2=173.7+0.10*(S2-2402)    
  CASE S2>9408.AND.S2<11809           
  U2=929.4+0.15*(S2-9408)             
  CASE S2>11808.AND.S2<16809            
  U2=1289.4+0.20*(S2-11808)             
  CASE S2>16808                       
  U2=2289.0+0.3*(S2-16808)              
                                                                             
   ENDC
   ENDC

U=U1+U2

OTHERWISE

     DO CASE

    CASE A>1663.AND.A<9409          
     U=0.12*(A-1663)                
    CASE A>9408.AND.A<11089        
     U=929.4+0.15*(A-9408)         
    CASE A>11808.AND.A<16809         
     U=1289.4+0.20*(A-11808)         
    CASE A>16808                    
     U=2289.4+0.3*(A-16808)          
                                                                            
     DO CASE                  
        CASE S2>369.AND.S2<555     
         U2=9.23+0.05*(S2-369)      
        case S2>554.AND.S2<925    
         U2=18.48+0.06*(S2-554)     
        CASE S2>924.AND.S2<2403    
         U2=40.68+0.09*(S2-924)    
        CASE S2>2402.AND.S2<4309   
         U2=173.7+0.10*(S2-2402)    
  CASE S2>9408.AND.S2<11809           
  U2=929.4+0.15*(S2-9408)             
  CASE S2>11808.AND.S2<16809            
  U2=1289.4+0.20*(S2-11808)             
  CASE S2>16808                       
  U2=2289.0+0.3*(S2-16808)              

         endc
U=MIN(U1,U2)
ENDC
endc
 U=round(U*(1-K)*100,0)
ENDI
sele a

case ras_mes='01'.OR.RAS_MES='11'.OR.RAS_MES='12'
sele a
if np='7'
U=0

ELSE

do case
case np='0'.or.np='1'
k=0
case np='2'.or.np='3'
k=30/100
case np='4'.or.np='5'
k=50/100
case np='6'
k=80/100
endcase
 A=INT((S1+S2)/100)
 S1=INT(S1/100)
 S2=INT(S2/100)
 SELE a

 DO CASE

  CASE RPV='2'   && совместители

        DO CASE                 
        CASE A=0               
        U=0                   
        CASE A<301             
        U=A*0.02              
        CASE A>300.AND.A<501     
        U=6.0+0.03*(A-300)     
        CASE A>500.AND.A<701    
        U=12+0.05*(A-500)     
        case A>700.AND.A<1301  
        U=22+0.06*(A-700)    
        CASE A>1300.AND.A<3301   
        U=58+0.09*(A-1300)   
        CASE A>3300.AND.A<6101  
        U=238+0.10*(A-3300)   
        CASE A>6100.AND.A<12301            
         U=518+0.11*(A-6000)                    
        CASE A>12300.AND.A<19101            
        U=1200+0.12*(A-12300)             
  CASE A>19100.AND.A<25201            
  U=2016+0.15*(A-19100)             
  CASE A>25200.AND.A<38001            
  U=2931+0.20*(A-25200)             
  CASE A>38000                       
  U=5491+0.3*(A-38000)              

   ENDC


  CASE  A<=2300                       
  U=0                                
  CASE S1>2300.AND.S1<19101            
  U=0.12*(S1-2300)                    
  CASE S1>19100.AND.S1<25201            
  U=2016+0.15*(S1-19100)             
  CASE S1>25200.AND.S1<38001            
  U=2931+0.20*(S1-25200)             
  CASE S1>38000                       
  U=5491+0.3*(S1-38000)              
  CASE S1>19100.AND.S1<25201            
  U=2016+0.15*(S1-19100)             
  CASE S1>25200.AND.S1<38001            
  U=2931+0.20*(S1-25200)             
  CASE S1>38000                       
  U=5491+0.3*(S1-38000)              
                                                                                 
        DO CASE                    
        CASE S2=0                 
         U2=0                     
        CASE S2<301                  
         U2=S2*0.02                  
        CASE S2>300.AND.S2<501       
         U2=6.0+0.03*(S2-300)        
          OTHERWISE                  
                                                                            
       DO CASE                       
        CASE S2>500.AND.S2<701    
        U2=12+0.05*(S2-500)     
        case S2>700.AND.S2<1301  
        U2=22+0.06*(S2-700)    
        CASE S2>1300.AND.S2<3301   
        U2=58+0.09*(S2-1300)   
        CASE S2>3300.AND.S2<6101  
        U2=238+0.10*(S2-3300)   
        CASE S2>6100.AND.S2<12301            
         U2=518+0.11*(S2-6000)                    
        CASE S2>12300.AND.S2<19101            
        U2=1200+0.12*(S2-12300)             
  CASE S2>19100.AND.S2<25201            
  U2=2016+0.15*(S2-19100)             
  CASE S2>25200.AND.S2<38001            
  U2=2931+0.20*(S2-25200)             
  CASE S2>38000                       
  U2=5491+0.3*(S2-38000)              
                                                                           
   ENDC
   ENDC
U=U1+U2

OTHERWISE

     DO CASE

    CASE A>2300.AND.A<19101         
     U=0.12*(A-2300)                
    CASE A>19100.AND.A<25201         
     U=2016+0.15*(A-19100)         
    CASE A>25200.AND.A<38001         
     U=2931+0.20*(A-25200)         
    CASE A>38000                    
     U=5491+0.3*(A-38000)          
                                                                           
     DO CASE                  
        CASE S2>500.AND.S2<701    
        U2=12+0.05*(S2-500)     
        case S2>700.AND.S2<1301  
        U2=22+0.06*(S2-700)    
        CASE S2>1300.AND.S2<3301   
        U2=58+0.09*(S2-1300)   
        CASE S2>3300.AND.S2<6101  
        U2=238+0.10*(S2-3300)   
        CASE S2>6100.AND.S2<12301            
         U2=518+0.11*(S2-6000)                    
        CASE S2>12300.AND.S2<19101            
        U2=1200+0.12*(S2-12300)             
  CASE S2>19100.AND.S2<25201            
  U2=2016+0.15*(S2-19100)             
  CASE S2>25200.AND.S2<38001            
  U2=2931+0.20*(S2-25200)             
  CASE S2>38000                       
  U2=5491+0.3*(S2-38000)              
    endc
U=MIN(U1,U2)
ENDC
endc
 U=round(U*(1-K)*100,0)
ENDI
SELE A
endcase
RETU
