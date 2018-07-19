PrintN("Press Y or N to continue")

Repeat
  ; Get the key being pressed, or a empty string.
  Key$=UCase(Inkey())
  ;
  ; To Reduce the problems with an active loop
  ; a Delay(1) will release the CPU for the rest
  ; of this quanta if no key where pressed.
  Delay(1)
Until   Key$="Y" Or Key$="N"
PrintN("The response was "+Key$)
