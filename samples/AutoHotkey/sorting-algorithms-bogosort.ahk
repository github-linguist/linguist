MsgBox % Bogosort("987654")
MsgBox % Bogosort("319208")
MsgBox % Bogosort("fedcba")
MsgBox % Bogosort("gikhjl")

Bogosort(sequence) {
  While !Sorted(sequence)
    sequence := Shuffle(sequence)
  Return sequence
}

Sorted(sequence) {
  Loop, Parse, sequence
  {
    current := A_LoopField
    rest := SubStr(sequence, A_Index)
    Loop, Parse, rest
    {
      If (current > A_LoopField)
        Return false
    }
  }
  Return true
}

Shuffle(sequence) {
  Max := StrLen(sequence) + 1
  Loop % StrLen(sequence) {
    Random, Num, 1, % Max - A_Index
    Found .= SubStr(sequence, Num, 1)
    sequence := SubStr(sequence, 1, Num-1) . SubStr(sequence, Num+1)
  }
  Return Found
}
