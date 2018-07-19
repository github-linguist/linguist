#digitCount = 4
Global Dim digits(#digitCount - 1) ;holds random digits

Procedure showDigits()
  Print(#CRLF$ + "These are your four digits: ")
  Protected i
  For i = 0 To #digitCount - 1
    Print(Str(digits(i)))
    If i < (#digitCount - 1)
      Print(", ")
    Else
      PrintN("")
    EndIf
  Next
  Print("24 = ")
EndProcedure

Procedure playAgain()
  Protected answer.s
  Repeat
    Print("Play again (y/n)? ")
    answer = LCase(Left(Trim(Input()), 1))
    Select answer
      Case "n"
        ProcedureReturn #False
      Case "y"
        ProcedureReturn #True
      Default
        PrintN("")
        Continue
    EndSelect
  ForEver
EndProcedure

Procedure allDigitsUsed()
  Protected i
  For i = 0 To #digitCount - 1
    If digits(i) <> 0
      ProcedureReturn #False
    EndIf
  Next
  ProcedureReturn #True
EndProcedure

Procedure isValidDigit(d)
  For i = 0 To #digitCount - 1
    If digits(i) = d
      digits(i) = 0
      ProcedureReturn #True
    EndIf
  Next
  ProcedureReturn #False
EndProcedure

Procedure doOperation(List op.c(), List operand.f())
  Protected x.f, y.f, op.c
  op = op(): DeleteElement(op())
  If op = '('
    ProcedureReturn #False ;end of sub-expression
  EndIf

  y = operand(): DeleteElement(operand())
  x = operand()
  Select op
    Case '+'
      x + y
    Case '-'
      x - y
    Case '*'
      x * y
    Case '/'
      x / y
  EndSelect
  operand() = x
  ProcedureReturn #True ;operation completed
EndProcedure

;returns error if present and the expression results in *result\f
Procedure.s parseExpression(expr.s, *result.Float)
  NewList op.c()
  NewList operand.f()
  expr = ReplaceString(expr, " ", "") ;remove spaces

  If Len(expr) = 0: *result\f = 0: ProcedureReturn "": EndIf ;no expression, return zero

  Protected *ech.Character = @expr, lastWasDigit, lastWasOper, parenCheck, c.c
  While *ech\c
    c = *ech\c
    Select c
      Case '*', '/', '-', '+'
        If Not lastWasDigit: ProcedureReturn "Improper syntax, need a digit between operators.": EndIf
        If ListSize(op()) And (FindString("*/", Chr(op()), 1) Or (FindString("+-", Chr(op()), 1) And FindString("+-", Chr(c), 1)))
          doOperation(op(), operand())
        EndIf
        AddElement(op()): op() = c
        lastWasOper = #True: lastWasDigit = #False
      Case '('
        If lastWasDigit: ProcedureReturn "Improper syntax, need an operator before left paren.": EndIf
        AddElement(op()): op() = c
        parenCheck + 1: lastWasOper = #False
      Case ')'
        parenCheck - 1: If parenCheck < 0: ProcedureReturn "Improper syntax, missing a left paren.": EndIf
        If Not lastWasDigit: ProcedureReturn "Improper syntax, missing a digit before right paren.": EndIf
        Repeat: Until Not doOperation(op(),operand())
        lastWasDigit = #True
      Case '1' To '9'
        If lastWasDigit: ProcedureReturn "Improper syntax, need an operator between digits.": EndIf
        AddElement(operand()): operand() = c - '0'
        If Not isValidDigit(operand()): ProcedureReturn "'" + Chr(c) + "' is not a valid digit.": EndIf
        lastWasDigit = #True: lastWasOper = #False
      Default
        ProcedureReturn "'" + Chr(c) + "' is not allowed in the expression."
    EndSelect
    *ech + SizeOf(Character)
  Wend

  If parenCheck <> 0 Or lastWasOper: ProcedureReturn "Improper syntax, missing a right paren or digit.": EndIf
  Repeat
    If Not ListSize(op()): Break: EndIf
  Until Not doOperation(op(),operand())
  *result\f = operand()
  ProcedureReturn "" ;no error
EndProcedure

Define success, failure, result.f, error.s, i
If OpenConsole()
  PrintN("The 24 Game" + #CRLF$)
  PrintN("Given four digits and using just the +, -, *, and / operators; and the")
  PrintN("possible use of brackets, (), enter an expression that equates to 24.")
  Repeat
    For i = 0 To #digitCount - 1
      digits(i) = 1 + Random(8)
    Next

    showDigits()
    error = parseExpression(Input(), @result)
    If error = ""
      If Not allDigitsUsed()
        PrintN( "Wrong! (you didn't use all digits)"): failure + 1
      ElseIf result = 24.0
        PrintN("Correct!"): success + 1
      Else
        Print("Wrong! (you got ")
        If result <> Int(result)
          PrintN(StrF(result, 2) + ")")
        Else
          PrintN(Str(result) + ")")
        EndIf
        failure + 1
      EndIf
    Else
      PrintN(error): failure + 1
    EndIf
  Until Not playAgain()

  PrintN("success:" + Str(success) + " failure:" + Str(failure) + " total:" + Str(success + failure))

  Print(#CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
