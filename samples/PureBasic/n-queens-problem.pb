Global solutions

Procedure showBoard(Array queenCol(1))
  Protected row, column, n = ArraySize(queenCol())

  PrintN(" Solution " + Str(solutions))
  For row = 0 To n
    For column = 0 To n
      If queenCol(row) = column
        Print("|Q")
      Else
        Print("| ")
      EndIf
    Next
    PrintN("|")
  Next
EndProcedure

Macro advanceIfPossible()
  x + 1
  While x <= n And columns(x): x + 1: Wend
  If x > n
    ProcedureReturn #False ;backtrack
  EndIf
EndMacro

Procedure placeQueens(Array queenCol(1), Array columns(1), row = 0)
  Protected n = ArraySize(queenCol())

  If row > n
    solutions + 1
    showBoard(queenCol())
    ProcedureReturn #False ;backtrack
  EndIf

  Protected x, queen, passed
  While columns(x): x + 1: Wend

  ;place a new queen in one of the available columns
  Repeat
    passed = #True
    For queen = 0 To row - 1
      If ((queenCol(queen) - x) = (queen - row)) Or ((queenCol(queen) - x) = -(queen - row))
        advanceIfPossible()
        passed = #False
        Break ;ForNext loop
      EndIf
    Next

    If passed
      queenCol(row) = x: columns(x) = 1
      If Not placeQueens(queenCol(), columns(), row + 1)
        columns(x) = 0
        advanceIfPossible()
      EndIf
    EndIf
  ForEver
EndProcedure

Procedure queens(n)
  If n > 0
    Dim queenCol(n - 1)
    Dim columns(n - 1)
    placeQueens(queenCol(), columns())
  EndIf
EndProcedure

If OpenConsole()
  Define i
  For i = 1 To 12
    solutions = 0
    queens(i)
    PrintN(#CRLF$ + Str(solutions) + " solutions found for " + Str(i) + "-queens.")
    Input()
  Next

  Print(#CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
