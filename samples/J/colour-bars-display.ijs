   load 'viewmat'
   size=: 2{.".wd'qm' NB. J6
   size=: getscreenwh_jgtk_ '' NB. J7
   'rgb'viewmat (|.size){. (>.&.(%&160)|.size)$ 20# 256#.255*#:i.8
