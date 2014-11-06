sele a
use ssp
ks='    '
do while.not.eof()
ks=ksp
sele b
use base&ks
do while.not.eof()
replace kpm with 0
skip
enddo
use
sele a
skip
ks=ksp
enddo
