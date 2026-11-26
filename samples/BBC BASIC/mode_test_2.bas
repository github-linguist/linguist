   10 REM Mode Test
   20 :
   30 NUMMODES=3
   40 :
   50 FOR M%=0 TO NUMMODES
   60   MODE M%
   70   PROC_testCard
   80   PRINT TAB(2,2);"Mode: ";M%
   90   PROC_displayInfo(2,4)
  100   A%=INKEY(2000)
  110 NEXT
  120 END
  130 :
  140 DEF FN_getByteVDP(var%)
  150 A%=&A0
  160 L%=var%
  170 =USR(&FFF4)
  180 :
  190 DEF FN_getWordVDP(var%)
  200 =FN_getByteVDP(var%)+256*FN_getByteVDP(var%+1)
  210 :
  220 DEF PROC_displayInfo(x%,y%)
  230 W%=FN_getWordVDP(&0F)
  240 H%=FN_getWordVDP(&11)
  250 C%=FN_getByteVDP(&13)
  260 R%=FN_getByteVDP(&14)
  270 D%=FN_getByteVDP(&15)
  280 PRINT TAB(x%,y%+0)"W:"W%
  290 PRINT TAB(x%,y%+1)"H:"H%
  300 PRINT TAB(x%,y%+2)"C:"C%
  310 PRINT TAB(x%,y%+3)"R:"R%
  320 PRINT TAB(x%,y%+4)"#:"D%
  330 ENDPROC
  340 :
  350 DEF PROC_testCard
  360 FOR X%=0 TO 1279 STEP 20
  370   GCOL 0,X%/20
  380   MOVE X%,0:MOVE X%+20,0:PLOT 80,X%+20,80
  390   MOVE X%,0:MOVE X%,80:PLOT 80,X%+20,80
  400 NEXT
  410 GCOL 0,15
  420 MOVE 0,0:DRAW 1279,0: DRAW 1279,1023: DRAW 0,1023: DRAW 0,0
  430 MOVE 640,512: PLOT 148,640,1000
  440 FOR X%=0 TO 1279 STEP 80
  450   MOVE X%,0: DRAW X%,1023
  460 NEXT
  470 FOR Y%=0 TO 1023 STEP 80
  480   MOVE 0,Y%: DRAW 1279,Y%
  490 NEXT
  500 ENDPROC
