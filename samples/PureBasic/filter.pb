Dim Tal.i(9)
Dim Evens.i(0)

;- Set up an array with random numbers
For i=0 To ArraySize(Tal())
  Tal(i)=Random(100)
Next

;- Pick out all Even and save them
j=0
For i=0 To ArraySize(Tal())
  If Tal(i)%2=0
    ReDim Evens(j)    ; extend the Array as we find new Even's
    Evens(j)=tal(i)
    j+1
  EndIf
Next

;- Display the result
PrintN("List of Randoms")
For i=0 To ArraySize(Tal())
  Print(Str(Tal(i))+" ")
Next
PrintN(#CRLF$+#CRLF$+"List of Even(s)")
For i=0 To ArraySize(Evens())
  Print(Str(Evens(i))+" ")
Next
