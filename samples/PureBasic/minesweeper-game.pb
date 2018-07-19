Structure cell
  isMine.i
  display.c ;character to displays for cell, one of these {'.', '?', ' ', #)
EndStructure

Global Dim grid.cell(0,0)
Global mineCount, minesMarked, isGameOver

Procedure makeGrid(n, m)
  Protected rm, x, y
  Dim grid.cell(n - 1, m - 1)
  mineCount = n * m * (Random(4) + 2) / 10
  If mineCount < 0: mineCount = 1: EndIf

  For x = 0 To n - 1
    For y = 0 To m - 1
      grid(x, y)\display = '.'
    Next
  Next

  rm = mineCount
  While rm
    x = Random(n - 1)
    y = Random(m - 1)
    If Not grid(x, y)\isMine
      rm - 1: grid(x, y)\isMine = #True
    EndIf
  Wend
  minesMarked = 0
  isGameOver = #False
EndProcedure

Procedure displayGrid(isEndOfGame = #False)
  #lMargin = 4
  Protected x, y, display.s
  If Not isEndOfGame
    PrintN("Grid has " + Str(mineCount) + " mines, " + Str(minesMarked) + " mines marked.")
  EndIf
  PrintN(Space(#lMargin + 1) + ReplaceString(Space(ArraySize(grid(), 1) + 1), " ", "-"))
  For y = 0 To ArraySize(grid(), 2)
    Print(RSet(Str(y + 1), #lMargin, " ") + ":")
    For x = 0 To ArraySize(grid(), 1)
      Print(Chr(grid(x,y)\display))
    Next
    PrintN("")
  Next
EndProcedure

Procedure endGame(msg.s)
  Protected ans.s
  isGameOver = #True
  PrintN(msg): Print("Another game (y/n)?"): ans = Input()
  If LCase(Left(Trim(ans),1)) = "y"
    makeGrid(6, 4)
  EndIf
EndProcedure

Procedure resign()
  Protected x, y, found
  For y = 0 To ArraySize(grid(), 2)
    For x = 0 To ArraySize(grid(), 1)
      With grid(x,y)
        If \isMine
          If \display = '?'
            \display = 'Y'
            found + 1
          ElseIf \display <> 'x'
            \display = 'N'
          EndIf
        EndIf
      EndWith
    Next
  Next
  displayGrid(#True)
  endGame("You found " + Str(found) + " out of " + Str(mineCount) + " mines.")
EndProcedure

Procedure usage()
  PrintN("h or ? - this help,")
  PrintN("c x y  - clear cell (x,y),")
  PrintN("m x y  - marks (toggles) cell (x,y),")
  PrintN("n      - start a new game,")
  PrintN("q      - quit/resign the game,")
  PrintN("where x is the (horizontal) column number and y is the (vertical) row number." + #CRLF$)
EndProcedure

Procedure markCell(x, y)
  If grid(x, y)\display = '?'
    minesMarked - 1: grid(x, y)\display = '.'
  ElseIf grid(x, y)\display = '.'
    minesMarked + 1: grid(x, y)\display = '?'
  EndIf
EndProcedure

Procedure countAdjMines(x, y)
  Protected count, i, j
  For j = y - 1 To y + 1
    If j >= 0 And j <= ArraySize(grid(), 2)
      For i = x - 1 To x + 1
        If i >= 0 And i <= ArraySize(grid(), 1)
          If grid(i, j)\isMine
            count + 1
          EndIf
        EndIf
      Next
    EndIf
  Next

  ProcedureReturn count
EndProcedure

Procedure clearCell(x, y)
  Protected count
  If x >= 0 And x <= ArraySize(grid(), 1) And y >= 0 And y <= ArraySize(grid(), 2)
    If grid(x, y)\display = '.'
      If Not grid(x,y)\isMine
        count = countAdjMines(x, y)
        If count
          grid(x, y)\display = Asc(Str(count))
        Else
          grid(x, y)\display = ' '
          clearCell(x + 1, y)
          clearCell(x + 1, y + 1)
          clearCell(x    , y + 1)
          clearCell(x - 1, y + 1)
          clearCell(x - 1, y)
          clearCell(x - 1, y - 1)
          clearCell(x    , y - 1)
          clearCell(x + 1, y - 1)
        EndIf
      Else
        grid(x, y)\display = 'x'
        PrintN("Kaboom!  You lost!")
        resign()
      EndIf
    EndIf
  EndIf
EndProcedure

Procedure testforwin()
  Protected x, y, isCleared
  If minesMarked = mineCount
    isCleared = #True
    For x = 0 To ArraySize(grid(), 1)
      For y = 0 To ArraySize(grid(), 2)
        If grid(x, y)\display = '.': isCleared = #False: EndIf
      Next
    Next
  EndIf
  If isCleared: endGame("You won!"): EndIf
EndProcedure

If OpenConsole()
  Define action.s
  usage()
  makeGrid(6, 4): displayGrid()
  Repeat
    PrintN(""): Print(">"):  action = Input()
    Select Asc(LCase(Left(action, 1)))
      Case 'h', '?'
        usage()
      Case 'n'
        makeGrid(6, 4): displayGrid()
      Case 'c'
        clearCell(Val(StringField(action, 2, " ")) - 1, Val(StringField(action, 3, " ")) - 1)
        If Not isGameOver: displayGrid(): Else: testforwin(): EndIf
      Case 'm'
        markCell(Val(StringField(action, 2, " ")) - 1, Val(StringField(action, 3, " ")) - 1)
        displayGrid()
        testforwin()
      Case 'q'
        resign()
    EndSelect
  Until isGameOver

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
