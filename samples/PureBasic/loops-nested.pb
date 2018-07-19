; Creating and filling array
Dim Value(10, 5)
For a = 0 To 10
  For b = 0 To 5
    Value(a, b) = Random(19) + 1
  Next
Next
; iterating trough array
For a = 0 To 10
  For b = 0 To 5
    Debug Value(a, b)
    If Value(a, b) = 20
      ; 2 indicates, that there are two nested lopps to break out
      Break 2
    EndIf
  Next
Next
