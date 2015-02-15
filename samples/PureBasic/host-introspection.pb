Enumeration
  #LittleEndian
  #BigEndian
EndEnumeration

ProcedureDLL EndianTest()
  Protected Endian = #LittleEndian
  Protected dummy.l= 'ABCD'
  If "A"=Chr(PeekA(@dummy))
    Endian=#BigEndian
  EndIf
  ProcedureReturn Endian
EndProcedure

;- *** Start of test code
If OpenConsole()
  PrintN("Your word size is "+Str(SizeOf(Integer)) +" bytes,")
  Select EndianTest()
    Case #LittleEndian
      PrintN("and you use Little Endian.")
    Default
      PrintN("and you use Big Endian.")
  EndSelect
EndIf
