Procedure IsPangram_fast(String$)
  String$ = LCase(string$)
  char_a=Asc("a")
  ; sets bits in a variable if a letter is found, reads string only once
  For a = 1 To Len(string$)
    char$ = Mid(String$, a, 1)
    pos   = Asc(char$) - char_a
    check.l |  1 << pos
  Next
  If check & $3FFFFFF = $3FFFFFF
    ProcedureReturn 1
  EndIf
  ProcedureReturn 0
EndProcedure

Procedure IsPangram_simple(String$)
  String$ = LCase(string$)
  found   = 1
  For a = Asc("a") To Asc("z")
  ; searches for every letter in whole string
    If FindString(String$, Chr(a), 0) = 0
      found = 0
    EndIf
  Next
  ProcedureReturn found
EndProcedure

Debug IsPangram_fast("The quick brown fox jumps over lazy dogs.")
Debug IsPangram_simple("The quick brown fox jumps over lazy dogs.")
Debug IsPangram_fast("No pangram")
Debug IsPangram_simple("No pangram")
