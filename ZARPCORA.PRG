*ZarpCoRa 30/12/88 Заполнение расчетной информации 28.2.89,09:55 call curson/off-->set curs on/off
*08/12/89 14:44

parameters NextKod

setcolor(stdc)

rrr=1
first_lst=.t.

CLEAR

do while .t.

  SELE basef
  
  if first_lst
     setcolor(stdc)
     @ 1,1 SAY KODPOD
     @ 1,30 SAY NAMEPOD
     @ 2,1 SAY TN
     @ 2,30 SAY super->fio
     @ 3,18 SAY 'Расчетная информация'
     first_lst=.f.
  endif
  setcolor(stdc)
  @  4, 0 clear
  @  5, 2 say '-----Вход в Дополнительную Таблицу-------'
  @  6, 2 say 'Сумма для расчета 13-й зарплаты     '+str(spvz/100,9,2)
  @  7, 2 say 'Рассчитанная 13-я зарплата          '+;
              if(type('zarp13')='N',str(zarp13/100,9,2),'*нет поля*')
  @  8, 2 say 'Неполученная зарплата (депоненты)   '+str(znd/100,9,2)
  @  9, 2 say 'Номер платежной ведомоcти           '+str(npv,2)
  @ 10, 2 say 'Дата возникновения деп.задолжности  '+dz
  @ 11, 2 say 'Долг за работающим                  '+str(dr/100,9,2)
  @ 12, 2 say 'Копейки прошлого меcяца             '+str(kpm,2)
  @ 13, 2 say 'Среднемеcячная з/п из ФЗП           '+str(ozm/100,9,2)
  @ 14, 2 say 'Среднемеcячная з/п из ФМП           '+str(ohm/100,9,2)
  @ 15, 2 say '1/12 cуммы премий за прощлый год    '+str(dsp/100,9,2)
  @ 16, 2 say '1/12 13-ой з/п за позапрощлый год   '+str(stz/100,9,2)
  @ 17, 2 say 'Суммы для отпусков из Ф З П      1: '+str(ozb1/100,9,2)
  @ 18, 2 say 'трех  будущих месяцев            2: '+str(ozb2/100,9,2)
  @ 19, 2 say '                                 3: '+str(ozb3/100,9,2)
  @ 20, 2 say 'Суммы для отпусков из Ф М П      1: '+str(ohb1/100,9,2)
  @ 21, 2 say 'трех  будущих месяцев            2: '+str(ohb2/100,9,2)
  @ 22, 2 say '                                 3: '+str(ohb3/100,9,2)
  setcolor(mcolbln)
  @  5, 0 prompt ''
  @  6, 0 prompt ''
  @  7, 0 prompt ''
  @  8, 0 prompt ''
  @  9, 0 prompt ''
  @ 10, 0 prompt ''
  @ 11, 0 prompt ''
  @ 12, 0 prompt ''
  @ 13, 0 prompt ''
  @ 14, 0 prompt ''
  @ 15, 0 prompt ''
  @ 16, 0 prompt ''
  @ 17, 0 prompt ''
  @ 18, 0 prompt ''
  @ 19, 0 prompt ''
  @ 20, 0 prompt ''
  @ 21, 0 prompt ''
  @ 22, 0 prompt ''
  
  setcolor(ECOL)
  @ 23,0 clear
  @ 23,1 SAY '┘-ввод F1-помощь {Esc}/{Ключ}-выход {9}-предыдущий {3}-след.работник'
  @ 24,1 say 'F2-лицевой счет F3-постоянная F4-долговременная информация F6-Архив'
  setcolor(mcolbln)
  
  PressKey=0
  set key -1 to PressCr
  set key -2 to PressCr
  set key -3 to PressCr
  set key -5 to PressCr
  set key 18 to PressCr
  set key  3 to PressCr
  set key 27 to
  set key 28 to PressCr
  set escape on
  menu to rrr
  set curs off
  set key -1 to
  set key -2 to
  set key -3 to
  set key -5 to
  set key 18 to
  set key  3 to
  set key 28 to
  
  setcolor(stdc)
  @ 23,0 clear
  
  if PressKey=28
     keyboard chr(28)
  endif
  
  do case
  
    case PressKey=-1 .or. PressKey=-2 .or. PressKey=-3 .or. PressKey=-5
     
       NextKod=PressKey
       exit
       
    case PressKey=18 .or. PressKey=3
       
       do DnUpSotr
       loop
    
    case RRR=0
    
       EXIT
       
    case rrr=1
    
       do DopForma
       
    case rrr=2
       
       do GetChis with 6,'spvz',1
       
    case rrr=3 .and. type('zarp13')='N'
       
       do GetChis with 7,'zarp13',1
       
    case rrr=4
    
       do GetChis with 8,'znd',1
       
    case rrr=5
    
       do GetChis with 9,'npv',4
       
    case rrr=6
       
       do GetChis with 10,'dz',3
       
    case rrr=7
    
       do GetChis with 11,'dr',1
       
    case rrr=8
    
       do GetChis with 12,'kpm',4
       
    case rrr=9
    
       do GetChis with 13,'ozm',1
       
    case rrr=10
       
       do GetChis with 14,'ohm',1
       
    case rrr=11
       
       do GetChis with 15,'dsp',1
       
    case rrr=12
                                                                                                                                                                                                                                                               
       do GetChis with 16,'stz',1
       
    case rrr=13
       
       do GetChis with 17,'ozb1',1
       
    case rrr=14
                                                                                                                                                                                                                                                               
       do GetChis with 18,'ozb2',1
       
    case rrr=15
                                                                                                                                                                                                                                                               
       do GetChis with 19,'ozb3',1
       
    case rrr=16
                                                                                                                                                                                                                                                               
       do GetChis with 20,'ohb1',1
       
    case rrr=17
                                                                                                                                                                                                                                                               
       do GetChis with 21,'ohb2',1
       
    case rrr=18
                                                                                                                                                                                                                                                               
       do GetChis with 22,'ohb3',1
       
  endcase
  
enddo

setcolor(stdc)

RETURN

proc GetChis
para r,fldn,alg,pictf
priv i
if pcount()<4
   pictf='########.##'
endif

setcolor(ecol)
@ 24,0
@ 24,1 say 'Вводите'
setcolor(stdc)

set curs on

do case

  case alg=1
  
    i=&fldn/100
    @ r,38 get i picture '&pictf'
    read
    if lastkey()<>27
       repl &fldn with i*100
    endif
  
  case alg=2
    
    i=&fldn
    @ r,38 get i
    read
    if lastkey()<>27
       repl &fldn with i
    endif
    
  case alg=3
    
    i=&fldn
    @ r,38 get   i picture '9999';
           valid '*'+substr(i,3,4) $ '*01*02*03*04*05*06*07*08*09*10*11*12'
    read
    if lastkey()<>27
       repl &fldn with i
    endif
    
  case alg=4
    
    i=&fldn
    @ r,38 get i picture '##'
    read
    if lastkey()<>27
       repl &fldn with i
    endif

endcase

set curs off

return
*
