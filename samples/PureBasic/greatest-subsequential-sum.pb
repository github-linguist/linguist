If OpenConsole()
  Define s$, a, b, p1, p2, sum, max, dm=(?EndOfMyData-?MyData)
  Dim Seq.i(dm/SizeOf(Integer))
  CopyMemory(?MyData,@seq(),dm)

  For a=0 To ArraySize(seq())
    sum=0
    For b=a To ArraySize(seq())
      sum+seq(b)
      If sum>max
        max=sum
        p1=a
        p2=b
      EndIf
    Next
  Next

  For a=p1 To p2
    s$+str(seq(a))
    If a<p2
      s$+"+"
    EndIf
  Next
  PrintN(s$+" = "+str(max))

  Print("Press ENTER to quit"): Input()
  CloseConsole()
EndIf


DataSection
  MyData:
  Data.i  -1, -2, 3, 5, 6, -2, -1, 4, -4, 2, -1
  EndOfMyData:
EndDataSection
