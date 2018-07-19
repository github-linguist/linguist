Procedure.f PriceFraction(price.f)
  ;returns price unchanged if value is invalid
  Protected fraction
  Select price * 100
    Case 0 To 5
      fraction = 10
    Case 06 To 10
      fraction = 18
    Case 11 To 15
      fraction = 26
    Case 16 To 20
      fraction = 32
    Case 21 To 25
      fraction = 38
    Case 26 To 30
      fraction = 44
    Case 31 To 35
      fraction = 5
    Case 36 To 40
      fraction = 54
    Case 41 To 45
      fraction = 58
    Case 46 To 50
      fraction = 62
    Case 51 To 55
      fraction = 66
    Case 56 To 60
      fraction = 7
    Case 61 To 65
      fraction = 74
    Case 66 To 70
      fraction = 78
    Case 71 To 75
      fraction = 82
    Case 76 To 80
      fraction = 86
    Case 81 To 85
      fraction = 9
    Case 86 To 90
      fraction = 94
    Case 91 To 95
      fraction = 98
    Case 96 To 100
      fraction = 100
    Default
      ProcedureReturn price
  EndSelect

  ProcedureReturn fraction / 100
EndProcedure

If OpenConsole()
  Define x.f, i

  For i = 1 To 10
    x = Random(10000)/10000
    PrintN(StrF(x, 4) + " -> " + StrF(PriceFraction(x), 2))
  Next

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
