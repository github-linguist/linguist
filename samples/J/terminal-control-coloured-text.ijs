NB. relies on an vt100 terminal

CSI=: 27 91 { a.
'BLACK BLUE CYAN WHITE'=: 0 4 6 7
'OFF REVERSEVIDEO'=: 0 7

HIDECURSOR=: CSI,'?25l'
SHOWCURSOR=: CSI,'?25h'

csi=: (,~ (CSI , (' '&=)`(,:&';')}@:":))~
clear=: csi&'J'
attributes=: csi&'m'
color=: BLACK&$: : (attributes@:(40 30 + ,)) NB. BACKGROUND color FOREGROUND
move=: csi&'H'

upward=: csi&'A'
downward=: csi&'B'
foreward=: csi&'C'
backward=: csi&'D'

DB=: (downward , backward) ''

NB. J is character vector to simulate the J icon.
J=: (BLUE color WHITE[CYAN)
J=: J , (backward 1),' T ',(backward 1),DB,,3#,:'|',DB
J=: J , (backward 5),'*    |',DB
J=: J , (backward 5),'\____/'
smoutput(color BLACK),(clear 2),(move 8 22),J,(WHITE color BLACK),(downward 2)
