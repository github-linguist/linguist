Procedure.s escapeChars(text.s)
  Static specialChars.s = "[\^$.|?*+()"
  Protected output.s, nextChar.s, i, countChar = Len(text)
  For i = 1 To countChar
    nextChar = Mid(text, i, 1)
    If FindString(specialChars, nextChar, 1)
      output + "\" + nextChar
    Else
      output + nextChar
    EndIf
  Next
  ProcedureReturn output
EndProcedure

Procedure.s stripBlocks(text.s, first.s, last.s)
  Protected delimter_1.s = escapeChars(first), delimter_2.s = escapeChars(last)
  Protected expNum = CreateRegularExpression(#PB_Any, delimter_1 + ".*?" + delimter_2, #PB_RegularExpression_DotAll)
  Protected output.s = ReplaceRegularExpression(expNum, text, "")
  FreeRegularExpression(expNum)
  ProcedureReturn output
EndProcedure

Define source.s
source.s = "  /**" + #CRLF$
source.s + "   * Some comments" + #CRLF$
source.s + "   * longer comments here that we can parse." + #CRLF$
source.s + "   *" + #CRLF$
source.s + "   * Rahoo " + #CRLF$
source.s + "   */" + #CRLF$
source.s + "   function subroutine() {" + #CRLF$
source.s + "    a = /* inline comment */ b + c ;" + #CRLF$
source.s + "   }" + #CRLF$
source.s + "   /*/ <-- tricky comments */" + #CRLF$
source.s + "" + #CRLF$
source.s + "   /**" + #CRLF$
source.s + "    * Another comment." + #CRLF$
source.s + "    */" + #CRLF$
source.s + "    function something() {" + #CRLF$
source.s + "    }" + #CRLF$

If OpenConsole()
  PrintN("--- source ---")
  PrintN(source)
  PrintN("--- source with block comments between '/*' and '*/' removed ---")
  PrintN(stripBlocks(source, "/*", "*/"))
  PrintN("--- source with block comments between '*' and '*' removed ---")
  PrintN(stripBlocks(source, "*", "*"))

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
