parameters NextKod
SELE 0
USE SKR
SELE 0
USE SKPR
SELE 0
USE SKN
SELE 0
USE SNETR
SELE 0
USE SKVU
sele 0
use skvu
if file('supersrd.dbf') .and. type('basef->kalendar')='C'
   sele 0
   use supersrd
   get_kalen=.t.
else
   get_kalen=.f.
endif
SET COLOR TO &STDC
rrr=1
first_lst=.t.
CLEAR
do while .t.
  SELE basef
  set color to &stdc
  @ 0,1 SAY KODPOD
  @ 0,10 SAY NAMEPOD
  @ 0,50 SAY '������HH�� �H��������'
  @ 1,1 SAY TN
  @ 1,10 SAY super->fio
  set color to &mcolbln
  if get_kalen
     @  2, 0 prompt '*'
  endif
  @  3, 0 prompt '*'
  @  4, 0 prompt '*'
  @  5, 0 prompt '*'
  @  6, 0 prompt '*'
  @  7, 0 prompt '*'
  @  8, 0 prompt '*'
  @  9, 0 prompt '*'
  @ 10, 0 prompt '*'
  @ 11, 0 prompt '*'
  @ 12, 0 prompt '*'
  @ 13, 0 prompt '*'
  @ 14, 0 prompt '*'
  @ 15, 0 prompt '*'
  @ 16, 0 prompt '*'
  @ 17, 0 prompt '*'
  @ 18, 0 prompt '*'
  @ 19, 0 prompt '*'
  if type('basef->maxpen')='N'
     @ 20, 0 prompt '*'
  endif
  @ 21, 0 prompt '*'
  @ 22, 0 prompt '*'
  set color to &stdc
  do case
   case pts='2'
  @ 11, 2 say '��ᮢ�� �⠢��                     (�������) '+STR(OKS/100,11,2)
   case pts='3'
  @ 11, 2 say '������� �⠢��                     (��.���) '+STR(OKS/100,11,2)
   othe
  @ 11, 2 say '�����(�⠢��)                      (��.���) '+STR(OKS/100,11,2)
  endcase
  if first_lst
     set color to &stdc
     if get_kalen
        @  2, 2 say '��䨪 ࠡ���    '+kalendar
     endif
     @  3, 2 say 'K�� ��⥣�ਨ    '+kr
     @  4, 2 say 'K�� ����ᨨ   '+kp
     @  5, 2 say 'K�� ���������⥫�騪�   '+np
     @  6, 2 say '��� ஦�����           (������)  '+datr
     @  7, 2 say '��� ����㯫���� �� ࠡ���(����)  '+PR
     @  8, 2 say '��� ��砫� �����뢭��� �⠦�(����)  '+NS
     @  9, 2 say '�ਧ��� ��䭮� �⠢��(1-�����,2-��.,3-����)  '+PTS
     @ 10, 2 say '������� �����                                  '+STR(PA,7)
     @ 12, 2 say '����.��䢧��c�(1-��,0-���) '+CHP
     @ 13, 2 say '������⢮ ��⥩   '+str(K_det,2)
     @ 14, 2 say '�����(1-����.,2-ᮢ�.,0-��.��ॢ����.)'+RPV
     @ 15, 2 say '���⮢� ᡮ�    '+vp
     @ 16, 2 say 'K�� ��⥣�਩ ���ᨮ��஢ �� �������  '+pb
     @ 17, 2 say '����� ���⥦���� ����祭��             '+nomp
     @ 18, 2 say '�㬬� ���ᨨ                           '+str(spen/100,9,2)
     @ 19, 2 say '�����,-��.��������                    '+str(mpen/100,9,2)
     if type('basef->maxpen')='N'
       @ 20, 2 say '���ᨬ��쭠� �㬬� ��௫��� � ���ᨨ   '+str(maxpen/100,9,2)
     endif
     @ 21, 2 say '��騩��(1-誮��,2-���,3-��-��,4-����,5-�ᯨ࠭�)  '+STUD
     @ 22, 2 say 'K���஫� �� ���㤭����� (0-ࠡ�⠥� 1-㢮��� 2-�� ��c��� � �c���  '+YS
REPLACE KPSS WITH '2'     
     set color to &stdc
     @ 2,23
     if get_kalen
        if kalendar<>' '
           do LocaSpr with 2,23,basef->kalendar,'kod','naim','supersrd'
        endif
     endif
     @ 3,23
     IF kr<>' '
        do LocaSpr with 3,23,basef->kr,'kkp','naim','skr'
     endif
     set color to &stdc
     @ 4,22
     if kp<>' '
        do LocaSpr with 4,23,basef->kp,'kp','naim','skpr'
     endif
     set color to &stdc
     @ 5,29
     if np<>' '
        do LocaSpr with 5,29,basef->np,'kn','substr(naim,1,50)','skn'
     endif
     set color to &stdc
     @ 15,23
     if vp<>' '
        do LocaSpr with 15,23,basef->vp,'kyps','subs(naim,1,40)','skvu'
     endif
     set color to &stdc
    @ 16,44
     if pb<>' '
 *       do LocaSpr with 16,44,basef->pb,'kkvp','subs(naim,1,34)','svozr'
     endif
  endif
  first_lst=.f.
  SET COLOR TO &ECOL
  @ 23,0 clear
  @ 23,1 SAY '�-���� F1-������ {Esc}-��室 {9}-�।��騩 {3}-᫥�.ࠡ�⭨� '
  @ 24,1 say 'F2-��楢�� ��� F4-������६����� F5-���⭠� ���ଠ�� F6-��娢'
  set color to &mcolbln
  PressKey=0
  set key -1 to PressCr
  set key -3 to PressCr
  set key -4 to PressCr
  set key -5 to PressCr
  set key 18 to PressCr
  set key  3 to PressCr
  set key 27 to
  set key 28 to PressCr
  set escape on
  menu to rrr
  set curs off
  set key -1 to
  set key -3 to
  set key -4 to
  set key -5 to
  set key 18 to
  set key  3 to
  set key 28 to
  set color to &stdc
  @ 23,0 clear
  if PressKey=28
     keyboard chr(28)
  endif
  if rrr=0
     exit
  endif
  if get_kalen
     rrr=rrr-1
  endif
  if PressKey=-1 .or. PressKey=-3 .or. PressKey=-4 .or. PressKey=-5
       NextKod=PressKey
       exit
  elseif PressKey=18 .or. PressKey=3
       do DnUpSotr
       loop
  elseif type('basef->maxpen')<>'N' .and. rrr>17
     rrr=rrr+1
  endif
  do case
    case rrr=0
      do PrepGetP with 1
      spr_obl='supersrd'
      set key 28 to HelpKod
      r_h=2
      c_h=23
      txt_h='naim'
      @ r_h,19 GET kalendar;
               valid(tstkdolg('basef',spr_obl,'kalendar','kod',r_h,c_h,txt_h))
      READ
      @ r_h,19 say kalendar
      if kalendar=' '
         @ 2,23
      endif
      set key 28 to
    case rrr=1
      do PrepGetP with 1
      spr_obl='skr'
      set key 28 to HelpKod
      r_h=3
      c_h=23
      txt_h='naim'
      @ r_h,19 GET kr valid(tstkdolg('basef',spr_obl,'kr','kkp',r_h,c_h,txt_h))
      READ
      @ r_h,19 say kr
      if kr=' '
         @ 3,23
      endif
      set key 28 to
    case rrr=2
      do PrepGetP with 1
      spr_obl='skpr'
      set key 28 to HelpKod
      r_h=4
      c_h=23
      txt_h='naim'
      @ r_h,18 GET kp valid(tstkdolg('basef',spr_obl,'kp','kp',r_h,c_h,txt_h))
      READ
      @ r_h,18 say kp
      if kp=' '
         @ 4,23
      endif
      set key 28 to
    case rrr=3
      do PrepGetP with 1
      spr_obl='skn'
      set key 28 to HelpKod
      r_h=5
      c_h=29
      txt_h='substr(naim,1,50)'
      @ r_h,26 GET np valid(tstkdolg('basef',spr_obl,'np','kn',r_h,c_h,txt_h))
      READ
      @ r_h,26 say np
      if np=' '
         @ 5,29
      endif
      set key 28 to
    case rrr=4
      set color to &stdc
      do PrepGetP with 0,'������ ���� ஦����� � ���� ������'
      @ 6,36 GET datr picture '999999';
                    valid   (.not. (' ' $ datr);
                            .and. val(substr(datr,3,2))>0;
                            .and. val(substr(datr,3,2))<13;
                            .and. val(substr(datr,1,2))>0;
                            .and. val(substr(datr,1,2))<32)
      READ
      @ 24,0
      @ 6,36 SAY datr
    case rrr=5
      set color to &stdc
      do PrepGetP with 0,'������ ���� � ���� ����'
      @ 7,36 GET PR picture '9999';
                    valid   (.not. (' ' $ pr);
                            .and. val(substr(pr,3,2))>0;
                            .and. val(substr(pr,3,2))<13)
      READ
      @ 24,0
      @ 7,36 SAY PR
    case rrr=6
      do PrepGetP with 0,'������ ���� � ���� ����'
      @  8,40 GET NS picture '9999';
                    valid   (.not. (' ' $ ns);
                            .and. val(substr(ns,3,2))>0;
                            .and. val(substr(ns,3,2))<13)
      READ
      @ 24,0
      @  8,40 SAY NS
    case rrr=7
      do PrepGetP with 0
      @ 09,50 GET PTS valid (pts $ '123')
      READ
      @ 24,0
      @ 09,50 SAY PTS
    case rrr=8
      set curs on
      set color to &ecol
      @ 24,0
      @ 24,5 say '������ '
      set color to &stdc
      @ 10,50 GET PA picture '#######' valid (pa>=0)
      READ
      @ 24,0
      @ 10,50 SAY STR(PA,9)
    case rrr=9
      do PrepGetP with 0
      i=oks/100
      @ 11,45 GET i picture '#########.##' valid(i>=0)
      READ
      @ 24,0
      if lastkey()<>27
         repl oks with i*100
      endif
      @ 11,45 SAY STR(OKS/100,12,2)
    case rrr=10
      do PrepGetP with 0
      @ 12,30 GET CHP valid (chp $ '01')
      READ
      @ 24,0
      @ 12,30 SAY CHP

    case rrr=11
    
      do PrepGetP with 0
i=k_det
      @ 13,21 GET i picture '##' valid(i>=0)
      READ
if lastkey()#27
repl k_det with i
endif
      @ 24,0
      @ 13,21 SAY str(k_det,2)

    case rrr=12
      do PrepGetP with 0
      @ 14,41 GET RPV valid(rpv $ '012')
      READ
      @ 24,0
      @ 14,41 SAY RPV
      sele super
      repl rpv with basef->rpv
      sele basef
    case rrr=13
      do PrepGetP with 1
      spr_obl='skvu'
      set key 28 to HelpKod
      r_h=15
      c_h=23
      txt_h='substr(naim,1,40)'
      @ r_h,19 GET vp valid(tstkdolg('basef',spr_obl,'vp','kyps',r_h,c_h,txt_h))
      READ
      @ r_h,19 say vp
      if vp=' '
         @ 15,23
      endif
      set key 28 to
    case rrr=14
      do PrepGetP with 1
      spr_obl='svozr'
      set key 28 to HelpKod
      r_h=16
      c_h=44
      txt_h='substr(naim,1,34)'
      @ r_h,41 GET pb 
      READ
      @ r_h,41 say pb
      if pb=' '
         @ 16,41
      endif
      set key 28 to
    case rrr=15
      do PrepGetP with 0
      @ 17,41 GET nomp
      READ
      @ 24,0
      @ 17,41 SAY nomp
    case rrr=16
      do PrepGetP with 0
      i=spen/100
      @ 18,41 GET i picture '####.##' valid (spen>=0)
      READ
      @ 24,0
      if lastkey()<>27
         repl spen with i*100
      endif
      @ 18,41 say str(i,7,2)
    case rrr=17
      do PrepGetP with 0
      i=mpen/100
      @ 19,41 GET i picture '####.##' valid (mpen>=0)
      READ
      @ 24,0
      if lastkey()<>27
         repl mpen with i*100
      endif
      @ 19,41 say str(i,7,2)
    case rrr=18
      do PrepGetP with 0
      i=maxpen/100
      @ 20,41 GET i picture '####.##' valid (mpen>=0)
      READ
      @ 24,0
      if lastkey()<>27
         repl maxpen with i*100
      endif
      @ 20,41 say str(i,7,2)
    case rrr=19
      do PrepGetP with 0
      @ 21,55 GET STUD valid (stud $ ' 012345')
      READ
      @ 24,0
      @ 21,55 SAY STUD
    case rrr=20
      do PrepGetP with 0
      @ 22,75 GET YS valid (ys $ '012')
      READ
      @ 24,0
      @ 22,75 SAY YS
  endcase
  set curs off
  if get_kalen
     rrr=rrr+1
  endif
  if type('basef->maxpen')<>'N' .and. rrr>17
     rrr=rrr-1
  endif
  rrr=rrr+1
enddo
set color to &stdc
set curs off
SELE skr
USE
SELE skpr
USE
SELE skn
USE
SELE snetr
USE
SELE SKVU
USE
if get_kalen
   sele supersrd
   use
endif
sele basef
RETURN
procedure PrepGetP
parameter tp,tx
priv tx1
if pcount()<2
  tx1='������ '
else
  tx1=tx
endif
set colo to &ecol
@ 24,0
@ 24,3 say if(tp=1,'������ ��� ��� ������: F1-������, {Esc}/{K���}-�⪠�',tx1)
SET COLOR TO &stdc
set curs on
return
*
proc LocaSpr
para p1,p2,p3,p4,p5,p6
@ p1,p2
sele &p6
loca for p3=&p4
IF EOF()
   set color to &ecol
   @ p1,p2 SAY '***������ ���***'
   set color to &stdc
ELSE
   @ p1,p2 SAY &p5
ENDIF
sele basef
return

