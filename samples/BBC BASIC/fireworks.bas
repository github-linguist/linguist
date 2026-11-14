   10 REM Palette Animation Example
   20 :
   30 MODE 0
   40 C=1:Y=-1
   50 FOR I=0 TO 2000
   60   IF Y<0 X=640:Y=10:VX=COS(RND(1)*PI)*9:VY=20+RND(1)*30:MOVE X,Y
   70   X=X+VX
   80   Y=Y+VY
   90   VY=VY-1
  100   GCOL 0,C:C=1+(C MOD15)
  110   DRAW X,Y
  120 NEXT I
  130 O=3:C=2:P=1
  140 REPEAT 
  150   *FX19
  160   COLOUR C,9
  170   COLOUR P,1
  180   COLOUR O,0
  190   O=P:P=C:C=1+(C MOD15)
  200 UNTIL FALSE
