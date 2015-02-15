   load 'viewmat'
   size=. 2{.".wd'qm' NB. J6
   size=. getscreenwh_jgtk_ '' NB. J7
   rows=. (2^3+i.4),._1^i.4
   bars=. ((64%{.)#[:(<:@|%~i.)*/)"1 rows
   togreyscale=. (256#. [:<.255 255 255&*)"0
   'rgb' viewmat (4<.@%~{:size)# (64<.@%~{.size)#"1 togreyscale bars
