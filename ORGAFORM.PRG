*OrgaForm
*20/11/89 14:37

external OutfVEDO,OutfBIR,OutfBANK,OutfKRED,OutfPISP,OutfRees
external OutfSTUD,OutfKVAR,OutfSVET,OutfTEL,ZatrOutf,OutfVKAT,outfgost

PEREM=SPACE(20)

setcolor(stdc)
clear

NOM=1

DO WHILE .t.

   setcolor(bcol)
   @ 5,18,if(nar_mpo,20,19),70 box frame+' '
   @ 5,19 SAY 'Формы по организации в целом'
   setcolor(mcol)
   set key -2 to PressCr
   PressKey=0
   @  6,19 prompt ' 1.Сводная расчетная ведомость по заработной плате '
   @ 07,19 prompt ' 2.Удерж. по исполн.докум. в инспекцию испр.работ  '
   @ 08,19 prompt ' 3.Ведомость перечисления вкладов в сбербанк       '
   @ 09,19 prompt ' 4.Ведомость перечисления сумм за кредит           '
   @ 10,19 prompt ' 5.Ведомость удерж. по  прочим  исполн.документам  '
   @ 11,19 prompt ' 6.Алименты(реестр почт.перев.,перечисл.в сбербанк)'
   @ 12,19 prompt ' 7.Ведомость перечисления  зарплаты учащимся       '
   @ 13,19 prompt ' 8.Табуляграмма платежей за квартиру и комм.услуги '
   @ 14,19 prompt ' 9.Табуляграмма платежей за электроэнергию         '
   @ 15,19 prompt ' A.Табуляграмма платежей за телефон                '
   @ 16,19 prompt ' B.Формирование данныx для перечисления в Госстраx '
   @ 17,19 prompt ' C.Отнесение з/п по видам производственных затрат  '
   @ 18,19 prompt ' D.Сводная вед.расп.з/п по категориям и видам оплат'
   if nar_mpo
   @ 19,19 prompt ' E.Ведомость распределения з/п по видам работ      '
   endif
   setcolor(ecol)
   @ 23,0 clear
   keyboard ''
   @ 23,1 SAY 'Отметьте нужные формы, нажимая ┘/{ВВОД} в соответствующей строке,'
   @ 24,1 say 'затем нажмите F3 - печать отмеченных  или  {ESC}/{КЛЮЧ} -- ОТКАЗ '
   setcolor(mcol)
   menu to nom
   set key -2 to
   setcolor(stdc)
   @ 23,0 clear
   
   if lastkey()=27
      return
   elseif PressKey=-2
      exit
   endif
   
   nom1=nom
   
   PEREM=STUFF(PEREM,NOM1,1,IF(SUBS(PEREM,NOM1,1)='*',' ','*'))
   SET COLOR TO &STDC
   @ NOM1+5,8 SAY if(SUBS(PEREM,NOM1,1)='*','*выбрана*',space(9))
   nom=nom+1
   
ENDDO

setcolor(ecol)
@ 23,0  clear
@ 23,15 SAY '.....Ж д и т е.....'
close data

NakoProc=if(len(trim(substr(perem,2)))=0,'','nakoall')

if len(trim(left(perem,13)))<>0
   
   dele_str=''
   if substr(perem,2,1)='*'
      dele_str=dele_str+'*922'
   endif
   if substr(perem,3,1)='*'
      dele_str=dele_str+'*930'
   endif
   if substr(perem,4,1)='*'
      dele_str=dele_str+'*921'
   endif
   if substr(perem,5,1)='*'
      dele_str=dele_str+'*916*917*918'
   endif
   if substr(perem,7,1)='*'
      dele_str=dele_str+'*939'
   endif
   if substr(perem,8,1)='*'
      dele_str=dele_str+'*926*928'
   endif
   if substr(perem,9,1)='*'
      dele_str=dele_str+'*927*929'
   endif
   if substr(perem,10,1)='*'
      dele_str=dele_str+'*940'
   endif
   if substr(perem,11,1)='*'
      dele_str=dele_str+'*920'
   endif
   
   sele 0
   use bank
   add_kred=dele_str
   if substr(perem,2,10)=replicate('*',10)
      zap
   else
      dele_str=dele_str+if(substr(perem,6,1)='*','*919*941','')
      dele all for '*'+vu $ dele_str
      pack
   endif
   
   if substr(perem,5,1)='*'
      use bank7
      zap
   endif
   
   USE KREDIT
   zap
   
   sele 0
   use aliment
   if substr(perem,6,1)='*'
      zap
   endif
   
   sele 0
   use svo index svoindex
   lena=recc()
   
   declare kodna[lena], maskna[lena]
   do ReadMskM with kodna, maskna
   
   if substr(perem,1,1)="*" .or. substr(perem,6,1)='*'
   
      stor 0 to kopsv,ostsv,dolgsv,kolrabsv

      declare sumna[lena], dnei[lena],;
              sumnasv[lena], dneisv[lena]

      afill(sumnasv,0)
      afill(dneisv,0)

      sele 0
      use suder index suderind
      leyd=recc()

      declare kodyd[leyd], sumyd[leyd], kodydsv[leyd], sumydsv[leyd]
      afill(sumydsv,0)
      do ReadMskM with kodyd
      
   else
   
      sele svo
      use
      
   endif
   
   sele 0
   use pasp
   tek_mes=tm
   use
   
   if substr(perem,12,1)="*"
      do PrepZatr
   endif
   
   if substr(perem,13,1)='*'
      do PrepVKAT
   endif
   
   sele 0
   use ssp
   set filter to mes=tek_mes .and. z='+'
   go top

   do while .not. eof()

      setcolor(ecol)
      @ 24,0
      @ 24,1 say 'Подразделение '+ksp+' '+nbrig
      setcolor(stdc)
      ok_m=.f.
      dska=d
      kodpod=ksp
      namepod=nbrig
      kpodr=ksp
      do MountDsk with dska,kpodr,ok_m,.t.

      err_nakop=.f.

      if ok_m

        dm1=dm
        dm=dska
        do OpenABC with 'A','000'
        dm=dm1
        
        sele basef
        set filter to rpv $ '12' .and. ys $ '01'
        go top
        
        if substr(perem,1,1)='*'
        
           stor 0 to kop,ost,dolg,kolrab
           afill(sumna,0)
           afill(dnei,0)
           afill(sumyd,0)
           do FRscVedo with kolrab,kop,ost,dolg,kodna,sumna,dnei,lena,kodyd,;
                            sumyd,leyd,NakoProc
           for i=1 to lena
             sumnasv[i]=sumnasv[i]+sumna[i]
             dneisv[i] =dneisv[i] +dnei[i]
           next

           for i=1 to leyd
             sumydsv[i]=sumydsv[i]+sumyd[i]
           next
           kolrabsv = kolrabsv + kolrab
           kopsv    = kopsv    + kop
           ostsv    = ostsv    + ost
           dolgsv   = dolgsv   + dolg
        
        elseif len(NakoProc)<>0
        
           sele basef
           do while .not. eof()
              setcolor(ucol)
              @ 24,70 say tn
              do &NakoProc
              sele basef
              skip
           enddo
           
        endif
        
        if substr(perem,13,1)='*'
           sele basef
           set filter to rpv $ '012' .and. ys $ '01'
           go top
           do while ! eof()
              setcolor(ucol)
              @ 24,70 say tn
              do RecoVKAT
              sele basef
              skip
           enddo
           do VremVKAT with kodpod
        endif
        
        sele basef
        use
         
      endif
      
      if err_nakop
         close data
         return
      endif
      
      sele ssp
      skip

   enddo
   
   setcolor(stdc)
   @ 24,0
   
   if select('vrempere')<>0
      sele vrempere
      use
   endif
   sele ssp
   use
   
   if subs(perem,1,1)='*'
     do PrintFRM with 'OutfVEDO',;
        ' 1.Сводная расчетная ведомость по заработной плате '
   endif
   
   release kodna, sumna, dnei, sumnasv, dneisv, maskna,;
           kdyd, sumyd, kodydsv, sumydsv
   close data
   
   sele 0
   use superlist index supertn alias super
   
   sele 0
   use sorg
   
   sele 0
   use bank

   sele 0
   use kredit
   INDEX ON KO+NC+TN+VU TO KREDIN
   
   IF SUBS(PEREM,2,1)='*'
    DO PrintFRM with 'OutfBIR',;
       ' 2.Удерж. по исполн.докум. в инспекцию испр.работ  '
   ENDIF
                                                                                                                                                                                                                                                               
   IF SUBS(PEREM,3,1)='*'
    DO PrintFRM with 'OutfBANK',;
       ' 3.Ведомость перечисления вкладов в сбербанк       '
   ENDIF
                                                                                                                                                                                                                                                               
   IF SUBS(PEREM,4,1)='*'
    DO PrintFRM with 'OutfKRED',;
       ' 4.Ведомость перечисления сумм за кредит           '
   ENDIF
                                                                                                                                                                                                                                                               
   IF SUBS(PEREM,5,1)='*'
    DO PrintFRM with 'OutfPISP',;
       ' 5.Ведомость удерж. по  прочим  исполн.документам  '
   ENDIF
                                                                                                                                                                                                                                                               
   IF SUBS(PEREM,6,1)='*'
    DO PrintFRM with 'OutfRees',;
       ' 6.Алименты(реестр почт.перев.,перечисл.в сбербанк)'
   ENDIF
                                                                                                                                                                                                                                                               
   IF SUBS(PEREM,7,1)='*'
    DO PrintFRM with 'OutfSTUD',;
       ' 7.Ведомость перечисления  зарплаты учащимся       '
   ENDIF
                                                                                                                                                                                                                                                               
   IF SUBS(PEREM,8,1)='*'
    DO PrintFRM with 'OutfKVAR',;
       ' 8.Табуляграмма платежей за квартиру и комм.услуги '
   ENDIF
                                                                                                                                                                                                                                                               
   IF SUBS(PEREM,9,1)='*'
    DO PrintFRM with 'OutfSVET',;
       ' 9.Табуляграмма платежей за электроэнергию         '
   ENDIF
                                                                                                                                                                                                                                                               
   IF SUBS(PEREM,10,1)='*'
    DO PrintFRM with 'OutfTEL ',;
       ' A.Табуляграмма платежей за телефон                '
   ENDIF
   
   IF SUBS(PEREM,11,1)='*'
   
      sele kredit
      set filter to vu='920'
      go top
      
      do while .not. eof()
         
         ko_gos=ko
         it=0
         sum sku to it while ko=ko_gos
         IF .NOT. EOF()
          SKIP -1
         ELSE
          GO BOTTOM
         ENDIF
         SELE BANK
         APPE BLANK
         REPL BANK->VU WITH KREDIT->VU,BANK->KO WITH KREDIT->KO,;
              BANK->SU WITH IT
         sele kredit
         skip
      enddo
         
      sele kredit
      set filter to
    DO PrintFRM with 'Outfgost',;
       ' A.Ведомость перечисления в Госстрах               '
      
   ENDIF
   
   IF SUBS(PEREM,12,1)='*'
    DO PrintFRM with 'ZatrOutf',;
       ' C.Отнесение з/п по видам производственных затрат  '
   ENDIF

   IF SUBS(PEREM,13,1)='*'
    DO PrintFRM with 'OutfVKAT',;
       ' D.Сводная вед.расп.з/п по категориям и видам оплат'
   ENDIF
   
endif

CLOSE DATA

IF SUBS(PEREM,14,1)='*'

    DO PrintFRM with 'vrab',;
       ' E.Ведомость распределения з/п по видам работ      '
    close data

ENDIF

set proc to

RETURN
*
proc NakoAll
priv nu,vnu,wrk

sele basef
wrk='*'+uvu1+'*'+uvu2+'*'+uvu3+'*'+uvu4+'*'+uvu5+'*'+uvu6

if '*919' $ wrk .or. '*941' $ wrk
   
   stor 0 to RR3,ipdnl,itsvo
   
   for vnnu=1 to 12
      nnu=ltrim(str(vnnu,2))
      if KU&NNU='910'
         IPDNL=IPDNL+SKU&NNU
      endif
      if vo&nnu<>' '
         j=ascan(kodna,vo&nnu)
         if j>0 .and. j<=len(kodna)
            if substr(maskna[j],16,1)='1'
               itsvo=itsvo+svon&nnu
            endif
         endif
      endif
   next
   
endif

for vnu=1 to 6

   sele basef
   
   nu=ltrim(str(vnu))
   nu_oper=ltrim(str(vnu+6,2))
   
   IF '*'+BASEF->UVU&NU $ '*919*941'
   
      sele aliment
      
      if basef->ku&nu_oper=basef->uvu&nu
      
         rr3=0
 *        if ''=trim(basef->uko&nu) .or. dops_ukopoch==basef->uko&nu
          wrk=basef->sku&nu_oper
psb=0
do case
    case basef->vp='1'

*         psb=round(basef->sku&nu_oper/100,2)*100*0.03
 psb=round(basef->sku&nu_oper/100,2)*100*0.045
    psb=psb+psb*0.2 
    case basef->vp='2'
         psb=round(basef->sku&nu_oper/100,2)*100*0.05
    case basef->vp='3'
         psb=round(basef->sku&nu_oper/100,2)*100*0.03
endcase
*rr3=round(psb,-4)
 rr3=Round_(psb,-4)
         SELE ALIMENT
         APPE BLANK
         REPL NIL WITH basef->UND&NU,;
              SPP WITH if(rr3=0,0,basef->SKU&nu_oper+RR3),;
              SPS WITH RR3,kpa with basef->ukpa&nu,;
              ko with basef->uko&nu,its with itsvo, ipd with ipdnl,;
              sku with basef->sku&nu_oper,usu with basef->usu&nu,;
              uvu with basef->uvu&nu,ksp with ssp->ksp,tn with basef->tn
      endif
    
   elseif '*'+BASEF->UVU&NU $ add_kred  && dele_str
          **********'*920*921*922*930*916*917*918*927*929*940*926*928*939'
   
      SELE KREDIT
      
      IF DISKSPACE()<=RECSIZE()
      
         do ErrorTxt with 'Hет места на диске  '
         err_nakop=.t.
         RETURN
       
      ENDIF
      
      if basef->ku&nu_oper = basef->uvu&nu .and. basef->sku&nu_oper > 0
      
         APPE BLANK
         REPL VU WITH BASEF->UVU&NU,KO WITH BASEF->UKO&NU,NC WITH ssp->ksp,;
              TN WITH;
                 if(basef->uvu&nu='930'.and.len(trim(basef->ukpa&nu))<>0,;
                    basef->ukpa&nu,;
                    BASEF->TN),;
         ND WITH BASEF->UND&NU,HO WITH BASEF->UHO&NU,SO WITH BASEF->USO&NU,;
         SKU WITH BASEF->sku&nu_oper,;
         KPU WITH BASEF->UKNO&NU,;
         SVO WITH (BASEF->SVON1+BASEF->SVON2+BASEF->SVON3+BASEF->SVON4+;
         BASEF->SVON5+BASEF->SVON6+BASEF->SVON7+BASEF->SVON8+BASEF->SVON9+;
         BASEF->SVON10+BASEF->SVON11+BASEF->SVON12),dn with basef->rabd&mespod
         
      endif
      
   ENDIF
   
next

sele basef
if substr(perem,12,1)='*'
   do RecoZatr
endif
   
sele basef
return
*
proc PrintFRM
para nameproc,txtproc
priv count_prn

count_prn=1
 
do while .t.

   setcolor(stdc)
   clear
   @ 5,5 say txtproc
   Page_Size=66
   Page_Nom=1
   tp_out=KindOutF(tp_out)
   
   if tp_out=0
      
      setcolor(stdc)
      @ 6,0 clear
      setcolor(ecol)
      @ 24,0
      @ 24,5 say '.....Ж д и т е.....'
      setcolor(stdc)
      set cons off
      set print off
   
   endif
   
   do &nameproc

   SET PRINT OFF
   set cons on
   
   do case
      case tp_out=1 .and. prow()>1
         eject
      case tp_out=2
         do ErrorTxt with 'Конец формы...'
      case tp_out=0
         exit
   endcase
   
   OTV=' '
   do AskYesNo with otv,24,' Повторить ? ',ecol
   
   if otv<>'D'.or.lastkey()=27
      exit
   endif
   
   count_prn=count_prn+1
   
enddo
return
***************************************************************************
Func Round_
Para Summa,Kol
Private Mod,Ret,Int
Summa = Summa/100

Return ( Round(Summa,2)*100 )

Sign = iif(Summa<0,-1,1)
Summa = abs(Summa)
Int = int(Summa)
Mod = mod(int,1000)
Ret = int(int/1000)*1000
if Summa = 0
   Ret = 0
elseif Summa <= 700
   Ret = 500
elseif Summa <= 1000
   Ret = 1000
elseif Mod >= 0 .and. Mod < 300
   Ret = Ret + 0
elseif Mod >= 300 .and. Mod < 700
   Ret = Ret + 500
else
   Ret = Ret + 1000
end
Return ( Sign*Ret*100 )
