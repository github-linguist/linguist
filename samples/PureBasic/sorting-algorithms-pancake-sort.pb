If OpenConsole()
  Define i, j, k, Loops
  Dim Pile(9)
  ;--------------------------------------------------------------
  ;- Create a Random Pile()
  For i=1 To 9                             ;- Initiate the Pile
    Pile(i)=i
  Next
  For i=9 To 1 Step -1                     ;- Do a Fisher-Yates shuffle
    Swap Pile(i),Pile(Random(i-1)+1)
  Next
  Print("Random Pile()    :")
  For i=1 To 9
    Print(" "+Str(Pile(i)))
  Next
  ;--------------------------------------------------------------
  ;- Start Sorting
  For i=9 To 2 Step -1
    If Pile(i)<>i       ;- Only Flip it if the current cake need Swapping
      Loops+1
      j=0
      Repeat            ;- find place of Pancake(i) in the Pile()
        j+1
      Until Pile(j)=i

      For k=1 To (j/2)  ;- Flip it up
        Swap Pile(k),Pile(j-k+1)
      Next
      For k=1 To i/2    ;- Flip in place
        Swap Pile(k),Pile(i-k+1)
      Next

    EndIf
  Next

  Print(#CRLF$+"Resulting Pile() :")
  For i=1 To 9
    Print(" "+str(Pile(i)))
  Next
  Print(#CRLF$+"All done in "+str(Loops)+" loops.")
  Print(#CRLF$+#CRLF$+"Press ENTER to quit."): Input()
  CloseConsole()
EndIf
