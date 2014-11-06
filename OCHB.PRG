** очистка баз от начислений и удеpжаний
for l=10 to 23
l1=ltrim(str(l,2)) 

sele a
use base00&l1
do while.not.eof()
for i=1 to 12
j=ltrim(str(i,2))
replace svon&j with 0,dn&j with 0,vo&j with '   ',kmn&j with '  '
REPLACE SKU&J WITH 0,KU&J WITH '   ',KMU&J WITH '  '
next

skip
enddo
next
