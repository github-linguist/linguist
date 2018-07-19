DataSection
  puzzle:
  Data.s "394002670"
  Data.s "000300400"
  Data.s "500690020"
  Data.s "045000900"
  Data.s "600000007"
  Data.s "007000580"
  Data.s "010067008"
  Data.s "009008000"
  Data.s "026400735"
EndDataSection

#IsPossible = 0
#IsNotPossible = 1
#Unknown = 0
Global Dim sudoku(8, 8)
;-declarations
Declare readSudoku()
Declare displaySudoku()
Declare.s buildpossible(x, y, Array possible.b(1))
Declare solvePuzzle(x = 0, y = 0)

;-procedures
Procedure readSudoku()
  Protected a$, row, column

  Restore puzzle
  For row = 0 To 8
    Read.s a$
    For column = 0 To 8
      sudoku(column, row) = Val(Mid(a$, column + 1, 1))
    Next
  Next
EndProcedure

Procedure displaySudoku()
  Protected row, column
  Static border.s = "+-----+-----+-----+"
  For row = 0 To 8
    If row % 3 = 0: PrintN(border): EndIf
    For column = 0 To 8
      If column % 3 = 0: Print("|"): Else: Print(" "): EndIf
      If sudoku(column, row): Print(Str(sudoku(column, row))): Else: Print("."): EndIf
    Next
    PrintN("|")
  Next
  PrintN(border)
EndProcedure

Procedure.s buildpossible(x, y, Array possible.b(1))
  Protected index, column, row, boxColumn = (x / 3) * 3, boxRow = (y / 3) * 3
  Dim possible.b(9)

  For index = 0 To 8
    possible(sudoku(index, y)) = #IsNotPossible ;record possibles in column
    possible(sudoku(x, index)) = #IsNotPossible ;record possibles in row
  Next

  ;record possibles in box
  For row = boxRow To boxRow + 2
    For column = boxColumn To boxColumn + 2
      possible(sudoku(column, row)) = #IsNotPossible
    Next
  Next
EndProcedure

Procedure solvePuzzle(x = 0, y = 0)
  Protected row, column, spot, digit
  Dim possible.b(9)

  For row = y To 8
    For column = x To 8
      If sudoku(column, row) = #Unknown
        buildpossible(column, row, possible())

        For digit =  1 To 9
          If possible(digit) = #IsPossible
            sudoku(column, row) = digit
            spot = row * 9 + column + 1
            If solvePuzzle(spot % 9, spot / 9)
              Break 3
            EndIf
          EndIf
        Next

        If digit = 10
          sudoku(column, row) = #Unknown
          ProcedureReturn #False
        EndIf
      EndIf
    Next
    x = 0 ;reset column start point
  Next
  ProcedureReturn #True
EndProcedure

If OpenConsole()
  readSudoku()
  displaySudoku()
  If solvePuzzle()
    PrintN("Solved.")
    displaySudoku()
  Else
    PrintN("Unable to solve puzzle") ;due to bad starting data
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit")
  Input()
  CloseConsole()
EndIf
