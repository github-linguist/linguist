#times=1000000

Structure Item
  name.s
  prob.d
  Amount.i
EndStructure

If OpenConsole()
  Define i, j, d.d, e.d, txt.s
  Dim Mapps.Item(7)
  Mapps(0)\name="aleph": Mapps(0)\prob=1/5.0
  Mapps(1)\name="beth":  Mapps(1)\prob=1/6.0
  Mapps(2)\name="gimel": Mapps(2)\prob=1/7.0
  Mapps(3)\name="daleth":Mapps(3)\prob=1/8.0
  Mapps(4)\name="he":    Mapps(4)\prob=1/9.0
  Mapps(5)\name="waw":   Mapps(5)\prob=1/10.0
  Mapps(6)\name="zayin": Mapps(6)\prob=1/11.0
  Mapps(7)\name="heth":  Mapps(7)\prob=1759/27720.0

  For i=1 To #times
    d=Random(#MAXLONG)/#MAXLONG  ; Get a random number
    e=0.0
    For j=0 To ArraySize(Mapps())
      e+Mapps(j)\prob            ; Get span for current itme
      If d<=e                    ; Check if it is within this span?
        Mapps(j)\Amount+1        ; If so, count it.
        Break
      EndIf
    Next j
  Next i

  PrintN("Sample times: "+Str(#times)+#CRLF$)
  For j=0 To ArraySize(Mapps())
      d=Mapps(j)\Amount/#times
      txt=LSet(Mapps(j)\name,7)+" should be "+StrD(Mapps(j)\prob)+" is "+StrD(d)
      PrintN(txt+" | Deviatation "+RSet(StrD(100.0-100.0*Mapps(j)\prob/d,3),6)+"%")
  Next

  Print(#CRLF$+"Press ENTER to exit"):Input()
  CloseConsole()
EndIf
