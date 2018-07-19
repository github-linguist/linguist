Dim MyList(9)

Declare is_list_sorted()

If OpenConsole()
  Define score, indata, i, txt$

  For i=1 To 9         ;- Initiate the list
    MyList(i)=i
  Next
  While is_list_sorted()
    For i=1 To 9      ;- Do a Fisher–Yates shuffle
      Swap MyList(i), MyList(Random(i)+1)
    Next
  Wend

  ;- Start the Game
  Repeat
    score+1
    txt$=RSet(str(score), 3)+": "      ;- Show current list
    For i=1 To 9
      txt$+str(MyList(i))+" "
    Next
    Repeat                             ;- Get input & swap
      Print(txt$+"| How many numbers should be flipped? "): indata=Val(Input())
    Until indata>=1 And indata<=9      ;- Verify the input
    For i=1 To (indata/2)
      Swap MyList(i),MyList(indata-i+1)
    Next
  Until is_list_sorted()

  ;- Present result & wait for users input before closing down
  PrintN(#CRLF$+"You did it in "+str(score)+" moves")
  Print("Press ENTER to exit"): Input()
  CloseConsole()
EndIf

Procedure is_list_sorted()
  Protected i
  Shared MyList()
  For i=1 To 9
    If MyList(i)<>i
      ProcedureReturn #False
    EndIf
  Next
  ProcedureReturn #True
EndProcedure
