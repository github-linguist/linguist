Procedure.s Generate(N)
  For i=1 To N
    sample$+"[]"
  Next
  For i=Len(sample$)-1 To 2 Step -1
    r=Random(i-1)+1
    If r<>i
      a.c=PeekC(@sample$+r*SizeOf(Character))
      b.c=PeekC(@sample$+i*SizeOf(Character))
      PokeC(@sample$+r*SizeOf(Character), b)
      PokeC(@sample$+i*SizeOf(Character), a)
    EndIf
  Next
  ProcedureReturn sample$
EndProcedure

Procedure Balanced(String$)
  Protected *p.Character, cnt
  *p=@String$
  While *p\c
    If *p\c='['
      cnt+1
    ElseIf *p\c=']'
      cnt-1
      If cnt<0: Break: EndIf
    EndIf
    *p+SizeOf(Character)
  Wend
  If cnt=0
    ProcedureReturn #True
  EndIf
EndProcedure

;- Test code
OpenConsole()
For i=1 To 5
  TestString$ = Generate(i)
  Print(TestString$)
  If Balanced(TestString$)
    PrintN(" is balanced.")
  Else
    PrintN(" is not balanced")
  EndIf
Next
