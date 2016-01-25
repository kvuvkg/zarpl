para SayLine
priv i,i1,i2,j,sr,mm
vxod = 0
mm=1
if type('SayLine')<>'L'
    SayLine=.t.
endif
set color to &ecol
@ 24,0
@ 24,3 say '......Ждите...Идет расчет......'
set color to &stdc
declare allna[12], allud[12],alim[2]
alim[1]=0
alim[2]=0

sele basef

&& Все начисления
allna[1]=vo1
allna[2]=vo2
allna[3]=vo3
allna[4]=vo4
allna[5]=vo5
allna[6]=vo6
allna[7]=vo7
allna[8]=vo8
allna[9]=vo9
allna[10]=vo10
allna[11]=vo11
allna[12]=vo12

&& Все удержания
allud[1]=ku1
allud[2]=ku2
allud[3]=ku3
allud[4]=ku4
allud[5]=ku5
allud[6]=ku6
allud[7]=ku7
allud[8]=ku8
allud[9]=ku9
allud[10]=ku10
allud[11]=ku11
allud[12]=ku12

sele basef
all_mes=''
all_alim=''
all_shtr=''
stor 0 to sprof,spart, alimtst, sbertst, sbertst1, alim_sum, uchtst ,spartb,spartoc
stor 0 to sprof1,spart1,spartb1,spartoc1,k1
for i=1 to 12
    nu=if(i<10,str(i,1),str(i,2))
    if .not.('*'+kmn&nu $ all_mes) .and. kmn&nu<>'  '
        all_mes=all_mes+'*'+kmn&nu
    endif
    wrk=ku&nu
    do case
    case wrk='910' .or. wrk='911'
        repl sku&nu with 0
        sr=i
        if sr>0 .and. sr<13 .and. SayLine
            do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
        endif
    case wrk='917'                                      && штраф %
        all_shtr=all_shtr+str(i,2)
        repl sku&nu with 0
    case wrk='919'                                      && алименты
        all_alim=all_alim+str(i,2)  && alimtst=i
        repl sku&nu with 0
    case wrk='930'                                      && перечисления в сбербанк
        sbertst=i
        repl sku&nu with 0
    case wrk='939'                                      && перечисления учащимся
        uchtst=i
        repl sku&nu with 0
    case wrk='942'                                      && удержание за питание
        if i>6
            j=str(i-6,1)
            jj=kmu&nu
            repl sku&nu with TestSvon(if(uvu&j=wrk,usu&j*rabd&jj/100,0),-1)
            sr=i
            if sr>0 .and. sr<13 .and. SayLine
                do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
            endif
        endif
    endcase
    if vo&nu<>' '
        j=ascan(kodna,vo&nu)
        if j>0 .and. j<=len(kodna)
            m1=maskna[j]
            oplats=svon&nu
            if substr(m1,14,1)='1'
                sprof=sprof+oplats
            endif
            ras_mes = substr(all_mes,2,2)
            mesnach=kmn&nu
            &&*************     разные месяца    *********
            if MesPod = mesnach
                if substr(m1,13,1)='1'
                    spart=spart+oplats
                    k1 = 1
                endif

                if substr(m1,11,1)='1'
                    spartb =spartb+oplats
                    k1 = 1
                endif

                if substr(m1,19,1)='1'
                    spartoc =spartOC+oplats
                endif
            endif

            if MesPod # mesnach
                mesnach1 =  mesnach       
                if substr(m1,13,1)='1'
                    spart1 = spart1+oplats
                endif

                if substr(m1,11,1)='1'
                    spartb1 =spartb1+oplats
                endif

                if substr(m1,19,1)='1'
                    spartoc1 =spartOC1+oplats
                endif
            endif

            if substr(m1,16,1)='1'  &&  исп.лист
                alim_sum=alim_sum+oplats
            endif
        endif
    endif
next

sele basef
stor 0 to u3,u4,u5
if chp='1'
    do uprof   with sprof,u5
    sr=scanud(allud,'912',.t.,u5)     && проф.
    if sr>0 .and. sr<13 .and. SayLine
        do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
    endif
endif

if k1 = 1
    if kpss='2'
        do upart with spart,u4
        sr=scanud(allud,'913',.t.,u4)    && 3.6.
        if sr>0 .and. sr<13 .and. SayLine
            do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
        endif
    endif
    do upartz   with spart,u4
    sr=scanud(allud,'911',.t.,u4)    && boен.
    if sr>0 .and. sr<13 .and. SayLine
        do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
    endif
endif

do uGOS  with spartOC,u4
sr=scanud(allud,'945',.t.,u4)    && ГОС СОЦ НЕЩЯСТ СЛУЧАИ

if sr>0 .and. sr<13 .and. SayLine
    do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
endif

if pb # " " 
    spartb = 0  
endif

do uBezr  with spartb,u4
sr=scanud(allud,'914',.t.,u4)    && 2.6
if sr>0 .and. sr<13 .and. SayLine
    do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
endif

do upartz  with spartb,u4
sr=scanud(allud,'911',.t.,u4)    && Bоeные
if sr>0 .and. sr<13 .and. SayLine
    do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
endif

if  spart1 >0
    &&*****************if   MesPod # mesnach
    if kpss='2'

        do upart   with spart1,u4
        sr=scanud(allud,'913',.t.,u4,mesnach1 )         && 3.6.
        if sr>0 .and. sr<13 .and. SayLine
            do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
        endif
    endif
    
    do uGOS  with spartOC1,u4
    sr=scanud(allud,'945',.t.,u4,mesnach1)              && ГОС СОЦ НЕЩЯСТ СЛУЧАИ
    if sr>0 .and. sr<13 .and. SayLine
        do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
    endif
    if pb # " " 
        spartb1 = 0
    endif
    
    do uBezr  with spartb1,u4
    sr=scanud(allud,'914',.t.,u4,mesnach1)              && 2.6
    if sr>0 .and. sr<13 .and. SayLine
        do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
    endif
endif

imes=2

sele basef
rec_base=recno()
stor 0 to old_alim, new_alim
do while imes<len(all_mes)
    ras_mes = substr(all_mes,imes,2)
    stor 0 to old_t1,old_t2,old_at1,old_at2,old_ot2
    ok_mes=.f.
    ira=val(ras_mes)
    ime=val(mespod)
    i_raz=ime-ira
    if (i_raz>=0 .and. i_raz<9) .or. i_raz<-3
        i=ira-3
        j=ime-8
        i1=if(i<1,i+12,i)
        j1=if(j<1,j+12,j)
        im=if(i*j>=0,max(i1,j1),min(i1,j1))
        iend=ime
        do while im<>iend
            sele sebaf
            go (rec_base-1)*12 + if(im<1,im+12,im)
            do FiveSum with ras_mes,old_t1,old_t2,old_at1,old_at2,;
                                    old_ot2
            im=im+1
            if im>12
                im=im-12
            endif
        enddo
    endif
    sele basef
    new_t1 =old_t1
    new_t2 =old_t2
    new_at1=old_at1
    new_at2=old_at2
    new_ot2=old_ot2
    if rpv='2' .or. ys='1'                              && cовмеcтитель или уволенб но участ.в расч.
        old_t2=old_t2+old_t1
        old_t1=0
        old_at2=old_at2+old_at1
        old_at1=0
    endif
    stor 0 to old_1,old_2,old_3,old_o2
    do upd with old_t1,old_t2,old_1
    if old_ot2<>0
        do upd with 0,old_ot2,old_o2
        old_1=old_1+old_o2
    endif
    if skn->pmn<>1
        do ums     with old_t1,old_t2,old_2
    endif
    do FiveSum with ras_mes,new_t1,new_t2,new_at1,new_at2,;
                            new_ot2
    if rpv='2' .or. ys='1'                              && cовмеcтитель или уволенб но участ.в расч.
        new_t2=new_t2+new_t1
        new_t1=0
        new_at2=new_at2+new_at1
        new_at1=0
    endif
    stor 0 to new_1,new_2,new_3,new_o2
    do upd     with new_t1,new_t2,new_1
    if new_ot2<>0
        do upd  with 0,new_ot2,new_o2
        new_1=new_1+new_o2
    endif
    IF skn->PMN<>1
        do ums with new_t1,new_t2,new_2
    ENDIF
    sr=scanud(allud,'910',.t.,(new_1-old_1),ras_mes)
    if sr>0 .and. sr<13 .and. SayLine
        do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
    endif
    IF skn->PMN<>1
        sr=scanud(allud,'911',.t.,(new_2-old_2),ras_mes)
        if sr>0 .and. sr<13 .and. SayLine
            do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
        endif
    ENDIF
    old_alim=old_alim+old_3
    new_alim=new_alim+new_3
    imes=imes+3
enddo

for k=1 to 12
    nu=if(k<10,str(k,1),str(k,2))
    if ku&nu='970' .or. ku&nu='971'
                repl kmu&nu with '  ', ku&nu with '   ',;
                    sku&nu with 0
    endif
next
jj=1
alimtst=len(all_alim)

if alimtst>0
    s_pod=0
    for i=1 to 12
        j=ltrim(str(i))
        if ku&j='910'.or.ku&j='913'.or.ku&j='945'.or.ku&j='914'
            s_pod=s_pod+sku&j
        endif
    next
    s_alim=alim_sum-s_pod
    i=1
    do while i<=alimtst
        j=val(substr(all_alim,i,2))
        if j>6
            wrk=ltrim(str(j-6,2))
            if uvu&wrk='919'
                w1=ltrim(str(j))
                w2=s_alim*usu&wrk/10000
                if ''=trim(uko&wrk)
                    do case
                    case (w2-1021)/102=(w2-1021)/102
                        w2=w2
                    case (w2-1022)/102=(w2-1022)/102
                        w2=w2
                    endcase
                endif
                repl kmu&w1 with mespod, ku&w1 with '919',;
                    sku&w1 with round((TestSvon(w2,-1)/100),2)*100
                sr=j &&scanud(allud,'919',.t.,s1) 
                if sr>0 .and. sr<13 .and. SayLine
                    do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
                endif
                
                if w2#0.and.vp#'0'
                    alim[1]=Round_(round(w2/100,2)*if(basef->vp='1',0.04,0.073)*100)
                    alim[1]=alim[1]+alim[1]*0.2
                else
                    alim[2]=Round_(round(w2/100,2)*0.04*100)+Round_(round(w2/100,2)*0.04*100)*0.2
                endif
                
                for n=1 to 12
                    nu=if(n<10,str(n,1),str(n,2))
                    if ku&nu='   ' && .or. ku&nu=kvo_
                        repl kmu&nu with mespod, ku&nu with if(vp='3','971','970'),;
                            sku&nu with if(vp='3',(round_(alim[2],-4)),(round_(alim[1],-4)))
                        jj_=val(nu)
                        exit
                    endif
                next
                sr=jj_ &&scanud(allud,'919',.t.,s1)
                if sr>0 .and. sr<13 .and. SayLine
                    do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
                endif
            endif
        endif
        i=i+2
        mm=mm+1
    enddo
endif

if uchtst>0
    w1=ltrim(str(uchtst,2))
    s_uch=svon1+svon2+svon3+svon4+svon5+svon6+svon7+svon8+svon9;
            +svon10+svon11+svon12+kpm;
            -sku1-sku2-sku3-sku4-sku5-sku6-sku7-sku8-sku9-sku10;
            -sku11-sku12+sku&w1
    if s_uch>0
        wrk=ltrim(str(uchtst-6,2))
        if uvu&wrk='939'
            sr=uchtst &&scanud(allud,'939',.t.,s_uch)
            repl kmu&w1 with mespod, ku&w1 with '939',;
                sku&w1 with TestSvon(s_uch*usu&wrk/10000-1)
            if sr>0 .and. sr<13 .and. SayLine
                do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
            endif
        endif
    endif
endif

shtraftst=len(all_shtr)
if shtraftst>0
    s_shtraf=svon1+svon2+svon3+svon4+svon5+svon6+svon7+svon8+svon9;
                    +svon10+svon11+svon12+kpm
    i=1
    do while i<=shtraftst
        j=val(substr(all_shtr,i,2))
        if j>6
            wrk=ltrim(str(j-6))
            if uvu&wrk='917'
                w1=ltrim(str(j,2))
                sr=j   &&scanud(allud,'917',.t.,s1)
                repl kmu&w1 with mespod, ku&w1 with '917',;
                    sku&w1 with TestSvon(alim_sum*usu&wrk/10000,-1)
                if sr>0 .and. sr<13 .and. SayLine
                    do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
                endif
            endif
        endif
        i=i+2
    enddo
endif

if sbertst>6
    s_sber=kpm+svon1+svon2+svon3+svon4+svon5+svon6+svon7+svon8+;
            svon9+svon10+svon11+svon12-;
            (sku1+sku2+sku3+sku4+sku5+sku6+sku7+sku8+sku9+sku10+;
            sku11+sku12)
    if s_sber>0
        wrk=ltrim(str(sbertst-6))
        if uvu&wrk='930'
            if usu&wrk<>0
                s_sber=if(s_sber>=usu&wrk,usu&wrk,0)
            endif
            w1=ltrim(str(sbertst,2))
            repl kmu&w1 with mespod, ku&w1 with '930',;
                sku&w1 with s_sber
            sr=sbertst &&scanud(allud,'930',.t.,s_sber)
            if sr>0 .and. sr<13 .and. SayLine
                do SayNaUd with .f.,sr+end_shap,ltrim(str(sr,2))
            endif
        endif
    endif
endif

sele basef
set color to &stdc
do itogNU with itog_l,.t.,.f.
return

func ScanUd
    para arr,kod,tst_val,uder_p,mesud
    priv i,j,isp
    if pcount()<5
        i=ascan(arr,kod)
        mesud=mespod
    else
        i=1
        do while i<=len(arr)
            j=if(i<10,str(i,1),str(i,2))
            if ku&j=kod .and. kmu&j=mesud
                exit
            endif
            i=i+1
        enddo
    endif
    if (i<1 .or. i>len(arr)) .and. tst_val .and. uder_p<>0
        i=ascan(arr,space(len(kod)))
    endif
    if i>0 .and. i<=len(arr)
        j=if(i<10,str(i,1),str(i,2))
        if uder_p=0
            repl kmu&j with ' ', ku&j with ' ', sku&j with 0
        else
            repl kmu&j with mesud, ku&j with kod, sku&j with TestSvon(uder_p,-1)

        endif
        arr[i]=kmu&j
    else
        if tst_val .and. uder_p<>0
            ScanScr=savescreen(10,20,15,60)
            set color to &bcol
            @ 10,20,15,60 box framedbl+' '
            sele suder
            seek kod
            set color to &ecol
            @ 12,22 say 'Нет cтроки для удержания '
            set color to &stdc
            @ 13,22 say kod+' '+if(found(),naim,'')+'.Меcяц '+mesud+;
                        '. Сумма '+str(uder_p/100)
            set color to &ecol
            @ 14,22 say ' Запомните его и нажмите ┘ '
            keyboard ''
            inkey(0)
            sele basef
            i=-1
            restscreen(10,20,15,60,ScanScr)
        endif
    endif
    return i

proc FiveSum
    para mes,s1,s2,s3,s4,s5
    priv nu,i,j,m1,oplats
    for i=1 to 12
        nu=if(i<10,str(i,1),str(i,2))
        if mes=kmn&nu .and. vo&nu<>' '
            j=ascan(kodna,vo&nu)
            if j>0 .and. j<=len(kodna)
                m1=maskna[j]
                oplats=svon&nu
                if substr(m1,5,1)='1'                   &&  табл.1
                    s1=s1+oplats
                elseif substr(m1,6,1)='1'               &&  табл.2
                    s2=s2+oplats
                elseif substr(m1,7,1)='1'
                    s5=s5+oplats                        &&  только по 2-й таблице
                endif
            endif
        endif
    next
    return

proc Upd
    SET TALK OFF
    PARA Sym1,Sym2,U
    priv s1,s2,t1,t2,a,k,u1,u2,k_det1,l
    stor 0 to u1,u2,k_det1,l
    s1=Sym1
    s2=Sym2
        L=VAL(LTRIM(RAS_MES))
        K=skn->SPN/100
        A=INT((S1+S2)/100)
        S1=(S1/100)
        S2=INT(S2/100)

    DO CASE
    CASE SKN->SPN=5.AND.RPV='1'
        M5   = VAL(SUBSTR(NAL[L],3,19))
        Z15  = VAL(SUBSTR(NAL[L],22,19))
        Z_15 = VAL(SUBSTR(NAL[L],41,19))
        Z_115= VAL(SUBSTR(NAL[L],60,5))  
        Z25  = VAL(SUBSTR(NAL[L],65,19))  
        Z_25 = VAL(SUBSTR(NAL[L],84,19)) 
        Z_225= VAL(SUBSTR(NAL[L],103,5))
        Z35  = VAL(SUBSTR(NAL[L],108,19))
        Z_35 = VAL(SUBSTR(NAL[L],127,19))  
        Z_335= VAL(SUBSTR(NAL[L],146,5))  
        Z45  = VAL(SUBSTR(NAL[L],151,19))  
        Z_45 = VAL(SUBSTR(NAL[L],170,19)) 
        Z_445= VAL(SUBSTR(NAL[L],189,5))
        Z55  = VAL(SUBSTR(NAL[L],194,19))
        Z_55 = VAL(SUBSTR(NAL[L],213,19))  
        Z_555= VAL(SUBSTR(NAL[L],232,5))  
        Z65  = VAL(SUBSTR(NAL[L],237,19)) 
        Z_65 = VAL(SUBSTR(NAL[L],256,19))  
        Z_665= VAL(SUBSTR(NAL[L],275,5)) 


        SELE basef

        s1 = s1 - 4*m5

        DO CASE
        CASE RPV='2'                                    && совместители
            DO CASE
            CASE A=0
                U=0
            CASE A<100000000
                U=A*0.2
            ENDC
            &&************************
        CASE  s1<=M5
            U=02
        CASE S1>(Z15-1).AND.S1<(Z25+1)
            U=Z_15+Z_115*(S1-(Z15-1))
        CASE S1>(Z25-1).AND.S1<(Z35+1)
            U=Z_25+Z_225*(S1-(Z25-1))
        CASE S1>(Z35-1).AND.S1<(Z45+1)
            U=Z_35+Z_335*(S1-(Z35-1))
        CASE S1>(Z45-1).AND.S1<(Z55+1)
            U=Z_45+Z_445*(S1-(Z45-1))
        CASE S1>(Z55-1).AND.S1<(Z65+1)
            U=Z_55+Z_555*(S1-(Z55-1))
        CASE S1>=Z65
            U=Z_65+Z_665*(S1-(Z65-1))
        ENDC
        U=round_(U*100,-4)
        sele basef

    CASE SKN->SPN=10.AND.RPV='1'

        M10=   VAL(SUBSTR(NAL[L],3,19))
        Z110=  VAL(SUBSTR(NAL[L],22,19))
        Z_110= VAL(SUBSTR(NAL[L],41,19))
        Z_1110=VAL(SUBSTR(NAL[L],60,5))  
        Z210=  VAL(SUBSTR(NAL[L],65,19))  
        Z_210= VAL(SUBSTR(NAL[L],84,19)) 
        Z_2210=VAL(SUBSTR(NAL[L],103,5))
        Z310=  VAL(SUBSTR(NAL[L],108,19))
        Z_310= VAL(SUBSTR(NAL[L],127,19))  
        Z_3310=VAL(SUBSTR(NAL[L],146,5))  
        Z410=  VAL(SUBSTR(NAL[L],151,19))  
        Z_410= VAL(SUBSTR(NAL[L],170,19)) 
        Z_4410=VAL(SUBSTR(NAL[L],189,5))
        Z510=  VAL(SUBSTR(NAL[L],194,19))
        Z_510= VAL(SUBSTR(NAL[L],213,19))  
        Z_5510=VAL(SUBSTR(NAL[L],232,5))  
        Z610  =VAL(SUBSTR(NAL[L],237,19)) 
        Z_610 =VAL(SUBSTR(NAL[L],256,19))  
        Z_6610=VAL(SUBSTR(NAL[L],275,5)) 

        SELE basef
        s1 = s1 - 9*m10
        DO CASE
        CASE RPV='2'                                    && Cовместители
            DO CASE
            CASE A=0
                U=0
            CASE A<100000000
                U=A*0.2
            ENDC
        CASE  s1<=M10
            U=02
        CASE S1>(Z110-0.01).AND.S1<(Z210+0.01)
            U=Z_110+Z_1110*(S1-(Z110-0.01))
        CASE S1>(Z210-0.01).AND.S1<(Z310+0.01)
            U=Z_210+Z_2210*(S1-(Z210-0.01))
        CASE S1>(Z310-0.01).AND.S1<(Z410+0.01)
            U=Z_310+Z_3310*(S1-(Z310-0.01))
        CASE S1>(Z410-0.01).AND.S1<(Z510+0.01)
            U=Z_410+Z_4410*(S1-(Z410-0.01))
        CASE S1>(Z510-0.01).AND.S1<(Z610+0.01)
            U=Z_510+Z_5510*(S1-(Z510-0.01))
        CASE S1>=Z610
            U=Z_610+Z_6610*(S1-(Z610-0.01))
        ENDC
        U=round_(U*100,-4)
        sele basef
    CASE SKN->SPN=0 .or. SKN->SPN>3
        ME  = SUBSTR(NAL[L],1,2)
        M   = VAL(SUBSTR(NAL[L],3,19))
        Z1  = VAL(SUBSTR(NAL[L],22,19))
        Z_1 = VAL(SUBSTR(NAL[L],41,19))
        Z_11= VAL(SUBSTR(NAL[L],60,5))  
        Z2  = VAL(SUBSTR(NAL[L],65,19))  
        Z_2 = VAL(SUBSTR(NAL[L],84,19)) 
        Z_22= VAL(SUBSTR(NAL[L],103,5))
        Z3  = VAL(SUBSTR(NAL[L],108,19))
        Z_3 = VAL(SUBSTR(NAL[L],127,19))  
        Z_33= VAL(SUBSTR(NAL[L],146,5))  
        Z4  = VAL(SUBSTR(NAL[L],151,19))  
        Z_4 = VAL(SUBSTR(NAL[L],170,19)) 
        Z_44= VAL(SUBSTR(NAL[L],189,5))
        Z5  = VAL(SUBSTR(NAL[L],194,19))
        Z_5 = VAL(SUBSTR(NAL[L],213,19))  
        Z_55= VAL(SUBSTR(NAL[L],232,5)) 
        Z6  = VAL(SUBSTR(NAL[L],237,19)) 
        Z_6 = VAL(SUBSTR(NAL[L],256,19))  
        Z_66= VAL(SUBSTR(NAL[L],275,5)) 

        if SKN->SPN> 3
            s1 = s1 - m*SKN->SPN
        Endif
        SELE basef
        
        if s1 <=1930.01 * k_det .and. k_det#0           &&  10*m
            s1 = s1 - k_det*1033.50 &&     - 609.00      &&     БАЛАШОВ ЮРА от 01.01.2014 г.
        endif
        if s1 <=1930.01 .and. k_det=0             
            s1 = s1 - 689.00                            &&     БАЛАШОВ ЮРА от 01.01.2014 г.
        endif
        if s1 <=1930.01 .and. NP='2'                    && ЧЕРНОБЫЛЕ 150              
            s1 = s1 - 1033.50                            &&     БАЛАШОВ ЮРА от 01.01.2014 г.
        endif
        
        do uBezr  with (spartb+spartb1),u4
        b =u4
        do upart   with (spart+spart1),u4
        c =u4
        do uGOS  with (spartOC+spartOC1),u4
        g =  u4
        s1 = s1 - c/100 - g/100 
        DO CASE
        CASE RPV='2'                                    && совместители
            DO CASE
            CASE A=0
                U=0
            CASE A<10000000000
                do uBezr  with spartb,u4
                b =u4
                do upart   with spart,u4
                c =u4
                do uGOS  with spartOC,u4
                g =  u4
                a = a - c/100 - b/100 
                U=a*0.18
            ENDCASE
            &&*********************
        CASE  s1<=0  
            U=0
        CASE S1>=0
            U=s1*0.18
        ENDC
        U=round_(U*100,-4)
        sele basef
    ENDCASE
    RETU

proc Ums
    SET TALK OFF
    PARA Sym1,Sym2,U
    priv s1,s2,a,u1,u2
    s1=Sym1
    s2=Sym2
    stor 0 to u1,u2,u
    IF skn->PMN=1
        U=0
    ELSE
        A=INT((S1+S2)/100)
        S1=INT(S1/100)
        S2=INT(S2/100)
        SELE basef
        DO CASE
        CASE A<=185
            U=0
        CASE a>=185.AND.S1<=205
            U=(A-185)*0.05
        CASE a>205
            u= 1+0.06*(a-205)
        OTHE
            do case
            case s1<=185
                u1=0
            case s1>185.and.s1<=205
                U1=0.05*(S1-185)
            case s1>205
                u1=1.0+0.06*(s1-205)
            endcase
            U2=S2*0.06
            U=U1+U2
        ENDCASE
        U=INT(U*100)
    ENDI
    sele basef
    RETU
    
&& 913 - пенсион со всех 3.6 процента
proc Upart
    SET TALK OFF
    PARA A1,U
    priv a
    a=a1
*    A=A/100
*    U=A*0.036
*    U=round_(U*100,-4)
    U = 0
    RETU

&& 911 - военн.
proc Upartz
    SET TALK OFF
    PARA A1,U
    priv a
    a=a1
    A=A/100
    U=A*0.015
    U=round_(U*100,-4)  
    RETU

&& 945 - ГОС СОЦ НЕЩЯСТ СЛУЧАИ
proc UGOS
    SET TALK OFF
    PARA A1,U
    priv a
    a=a1
*    A=A/100
*    U=A*0.02
*    U=round_(U*100,-4)
    u = 0
    RETU

&& 912 - проф.
proc Uprof
    SET TALK OFF
    PARA A1,U
    priv a
    a=a1
    *A=INT(A/100)
    A=A/100

    U=A*0.01
    U=round_(u*100,-4)
    RETU

&& 914
proc UBezr
    SET TALK OFF
    PARA A1,U
    priv a
    a=a1
*    A=A/100
*    U=A*0.026
*    U=round_(u*100,-4)
    U = 0
    RETU
