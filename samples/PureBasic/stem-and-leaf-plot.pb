If OpenConsole()
  Dim MyList(120)
  Define i, j, StemMax, StemMin
  Restore MyData          ; Get the address of MyData, e.g. the data to print as a Stem-and-leaf plot
  For a=0 To 120
    Read.i MyList(a)      ; Read the data into the used Array
    If MyList(a)>StemMax
      StemMax=MyList(a)   ; Find the largest Stem layer at the same time
    EndIf
    If MyList(a)<StemMin
      StemMin=MyList(a)   ; Find the smallest Stem layer at the same time
    EndIf
  Next
  StemMax/10: StemMin/10  ; Remove the leafs from the Stem limits
  SortArray(MyList(),#PB_Sort_Ascending)  ; Sort the data

  For i=StemMin To StemMax
    Print(RSet(Str(i),3)+" | ")           ; Print the Stem
    For j=0 To 120
      If MyList(j)<10*i                   ; Skip all smaller then current
        Continue
      ElseIf MyList(j)>=10*(i+1)          ; Break current print if a new Stem layer is reached
        Break
      Else
        Print(Str(MyList(j)%10)+" ")      ; Print all Leafs on this current Stem layer
      EndIf
    Next j
    PrintN("")
  Next i

  Print(#CRLF$+#CRLF$+"Press ENTER to exit")
  Input()
  CloseConsole()
EndIf

DataSection
MyData:
  Data.i  12,127, 28, 42, 39,113, 42, 18, 44,118, 44, 37,113,124, 37, 48,127, 36, 29, 31,125,139,131,115
  Data.i 105,132,104,123, 35,113,122, 42,117,119, 58,109, 23,105, 63, 27, 44,105, 99, 41,128,121,116,125
  Data.i  32, 61, 37,127, 29,113,121, 58,114,126, 53,114, 96, 25,109,  7, 31,141, 46, 13, 27, 43,117,116
  Data.i  27,  7, 68, 40, 31,115,124, 42,128, 52, 71,118,117, 38, 27,106, 33,117,116,111, 40,119, 47,105
  Data.i  57,122,109,124,115, 43,120, 43, 27, 27, 18, 28, 48,125,107,114, 34,133, 45,120, 30,127, 31,116,146
EndDataSection
