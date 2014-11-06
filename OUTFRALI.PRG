priv i,j,zps,blp,nu,sud,snach
   i=int(len(trim(vo1)+trim(vo2)+trim(vo3)+trim(vo4)+trim(vo5)+trim(vo6)+;
             trim(vo7)+trim(vo8)+trim(vo9)+trim(vo10)+trim(vo11)+trim(vo12)+;
             trim(ku1)+trim(ku2)+trim(ku3)+trim(ku4)+trim(ku5)+trim(ku6)+;
             trim(ku7)+trim(ku8)+trim(ku9)+trim(ku10)+trim(ku11)+trim(ku12);
            )/len(vo1);
        )+11

SET COLOR TO &STDC

if tp_out=2
   set cons on
   clear
   @ 0,0 say ''
else
   set cons off
endif

SELE basef

? replicate('-',81)

tk=TestKale() &&if(type('basef->kalendar')<>'C','',trim(basef->kalendar))
ZPS=pred_KP+' '+mesname+' '+TEK_GOD+',дн.'+STR(dn_arr&tk[val(mespod)],3)+;
             ' ,час.'+STR(chas_arr&tk[val(mespod)],6,2)

? '| '+ZPS+'  | '+ZPS+'  |'
ccTn = TN

ZPS=KODPOD+'  '+TN+'  '+super->FIO+'    '
? '| '+ZPS+'| '+ZPS+'|'

*ZPS='      H А Ч И С Л Е H О               '
 ZPS='   H А Ч И С Л Е H О   и н  '+STR(super->INDN,10,0)
? '| '+ZPS+'| '+ZPS+'|'

IF KPM<>0
   ZPS='Kопейки прошлого месяца '+STR(KPM/100,5,2)+'         '
   ? '|'+ZPS+' |'+ZPS+' |'
ENDIF

SNACH=0

for vnu=1 to 12

 nu=ltrim(str(vnu,2))

 IF VO&NU<>'   '

  SELE SVO
  seek basef->VO&NU

  SELE basef

*  i=timeconv(dn&nu,2)/100
 i=dn&nu/100
  ZPS=VO&NU+' '+left(SVO->NAIM,13)+' '+STR(SVON&NU/100,11,2)+;
      str(i,7,2)+' '+kmn&nu  && STR(i,j,2)+BLP
  STOR SNACH+SVON&NU TO SNACH
  ? '|'+ZPS+'|'+ZPS+'|'
 ENDIF

next

SNACH=SNACH+KPM
ZPS='  Итого начислено  '+STR(SNACH/100,12,2)+'        '

? '|'+ZPS+'|'+ZPS+'|'
? '|'+space(39)+'|'+space(39)+'|'

ZPS='       У Д Е Р Ж А H О                 '
? '|'+ZPS+'|'+ZPS+'|'

SUD=0

for vnu=1 to 12

 nu=ltrim(str(vnu,2))

 IF KU&NU<>'   '

  SELE SUDER
  seek basef->KU&NU

  SELE basef

  ZPS=KU&NU+' '+left(SUDER->NAIM,13)+STR(SKU&NU/100,11,2)+' '+;
      kmu&nu+'       '

  STOR SUD+SKU&NU TO SUD
  ? '|'+ZPS+' |'+ZPS+' |'

 ENDIF

next

ZPS='  Итого удержано   '+STR(SUD/100,12,2)+'        '
? '|'+ZPS+'|'+ZPS+'|'

OST=SNACH-SUD

IF OST>=0

 IF YS='1'

  OSP1='                 '
  OSP2=str(OST/100,11,2)

 ELSE

  OSP1='Остаток '+STR(OZ/100,5,2)
*  OSP2=str(INT(OST/100),11)
  OSP2=str(OST/100,11,2)

 ENDIF

 ZPS=OSP1+'  K выдаче '+OSP2+'   '

ELSE

 ZPS='Долг '+STR(CD/100,11,2)+'                       '

ENDIF

? '|'+ZPS+' |'+ZPS+' |'
? replicate('-',81)
****************************
Sele VedArx
Seek ccTn
i = 0
if Found()
 *  nDolg = Substr(dtos(Tek_Date),5,2)
 *  nDolg= "2000&nDolg"
  nDolg =Tek_Date
   Do while !eof() .and. VedArx->Tn = ccTn


     if Substr(dtos(VedArx->DataV),1,6) = nDolg
         i = i + 1
         if i = 1
             ZPS='  Погашение задолженности        '+VedArx->tn
             ? '|'+ZPS+'|'+ZPS+'|'
         Endif
         SELE SUDER
         seek VedArx->Kvu
         SELE VEDARX
         ZPS=" "+substr(dtoc(VedArx->Data),4)+" "+;
               VedArx->Kvu+" "+Suder->Naim+" "+str(VedArx->SumkVip,12,2)
         ? '|'+ZPS+'|'+ZPS+'|'
     Endif
     Skip 1
   EndDo
End
****************************
Sele DolgPr
Seek ccTn

if Found()
   nDolg = 0
   i = 0
   Do while !eof() .and. DolgPr->Tn = ccTn
     if DolgPr->SumDolg - DolgPr->SumVip > 0
       i = i + 1
       if i = 1
          ZPS='  Задолженность по зара плате    '+DolgPr->tn
          ? '|'+ZPS+'|'+ZPS+'|'
       Endif
       nDolg = nDolg + (DolgPr->SumDolg - DolgPr->SumVip)
       ZPS=" "+substr(dtoc(DolgPr->Data),4)+"   "+str(DolgPr->SumDolg - DolgPr->SumVip,15,2)+;
           space(15)
       ? '|'+ZPS+'|'+ZPS+'|'
     Endif
     Skip 1
   EndDo
   ZPS=' Итого   '+str(nDolg,15,2)+space(15)
   ? '|'+ZPS+'|'+ZPS+'|'
   ? replicate('-',81)
End
? ' '
Sele BaseF

return
*