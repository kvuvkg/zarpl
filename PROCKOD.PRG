**************
* PROCKOD    *
*            *
* 28/11/89   *
**************
PARA KODI,KODO
N=LEN(KODI)
L=LEN(KODO)
S=0
I=1
DO WHIL I<=N
 J=0
 ASC=ASC(SUBS(KODI,I))*i
 DO WHIL J<N .AND. J<=L
  *S=S+(ASC-2*j)*10**J
  S=S+(ASC-3*j)*7**J
  J=J+2
 ENDD
 I=I+1
ENDD
KODO=STR(S-INT(S/10**L)*10**L,L)
RETU

