For i=105 To 115
  Bin$=RSet(Bin(i),8,"0") ;- Convert to wanted type & pad with '0'
  Hex$=RSet(Hex(i),4,"0")
  Dec$=RSet(Str(i),3)
  PrintN(Dec$+" decimal = %"+Bin$+" = $"+Hex$+".")
Next
