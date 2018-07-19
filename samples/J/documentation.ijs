NB. =========================================================
NB.*apply v apply verb x to y
apply=: 128!:2

NB. =========================================================
NB.*def c : (explicit definition)
def=: :

NB.*define a : 0 (explicit definition script form)
define=: : 0

NB.*do v name for ".
do=: ".

NB.*drop v name for }.
drop=: }.

   Note 1
Note accepts multi-line descriptions.
Definitions display the source.
)

   usleep
3 : '''libc.so.6 usleep > i i''&(15!:0) >.y'
