;code from the maze generation task is place here in its entirety before the rest of the code

Procedure displayMazePath(Array maze(2), List Path.POINT())
  Protected x, y, vWall.s, hWall.s
  Protected mazeWidth = ArraySize(maze(), 1), mazeHeight = ArraySize(maze(), 2)
  Protected Dim mazeOutput.mazeOutput(mazeHeight)
  Protected Dim mazeRow.mazeOutput(0)
  Static pathChars.s = "@^>v<"

  For y = 0 To mazeHeight
    makeDisplayMazeRow(mazeRow(), maze(), y): mazeOutput(y) = mazeRow(0)
  Next

  If ListSize(path())
    FirstElement(path())
    Protected prevPath.POINT = path()

    While NextElement(path())
      x = path()\x - prevPath\x
      y = path()\y - prevPath\y
      Select x
        Case -1: dirTaken = #dir_W
        Case 1: dirTaken = #dir_E
        Default
          If y < 0
            dirTaken = #dir_N
          Else
            dirTaken = #dir_S
          EndIf
      EndSelect
      hWall = mazeOutput(prevPath\y)\hWall
      mazeOutput(prevPath\y)\hWall = Left(hWall, prevPath\x * #cellDWidth + 2) + Mid(pathChars, dirTaken + 1, 1) + Right(hWall, Len(hWall) - (prevPath\x * #cellDWidth + 3))
      prevPath = path()
    Wend
    hWall = mazeOutput(prevPath\y)\hWall
    mazeOutput(prevPath\y)\hWall = Left(hWall, prevPath\x * #cellDWidth + 2) + Mid(pathChars, #dir_ID + 1, 1) + Right(hWall, Len(hWall) - (prevPath\x * #cellDWidth + 3))

    For y = 0 To mazeHeight
      PrintN(mazeOutput(y)\vWall): PrintN(mazeOutput(y)\hWall)
    Next
  EndIf
EndProcedure

Procedure solveMaze(Array maze(2), *start.POINT, *finish.POINT, List Path.POINT())
  Protected mazeWidth = ArraySize(maze(), 1), mazeHeight = ArraySize(maze(), 2)
  Dim visited(mazeWidth + 1, mazeHeight + 1) ;includes padding for easy border detection

  Protected i
  ;mark outside border as already visited (off limits)
  For i = 1 To mazeWidth
    visited(i, 0) = #True: visited(i, mazeHeight + 1) = #True
  Next
  For i = 1 To mazeHeight
    visited(0, i) = #True: visited(mazeWidth + 1, i) = #True
  Next

  Protected x = *start\x, y = *start\y, nextCellDir
  visited(x + offset(#visited, #dir_ID)\x, y + offset(#visited, #dir_ID)\y) = #True

  ClearList(path())
  Repeat
    If x = *finish\x And y = *finish\y
      AddElement(path())
      path()\x = x: path()\y = y
      Break ;success
    EndIf

    nextCellDir = #firstDir - 1
    For i = #firstDir To #numDirs
      If Not visited(x + offset(#visited, i)\x, y + offset(#visited, i)\y)
        If maze(x + offset(#wall, i)\x, y + offset(#wall, i)\y) & wallvalue(i) <> #Null
          nextCellDir = i: Break ;exit for/next search
        EndIf
      EndIf
    Next

    If nextCellDir >= #firstDir
      visited(x + offset(#visited, nextCellDir)\x, y + offset(#visited, nextCellDir)\y) = #True

      AddElement(path())
      path()\x = x: path()\y = y

      x + offset(#maze, nextCellDir)\x: y + offset(#maze, nextCellDir)\y
    ElseIf ListSize(path()) > 0
      x = path()\x: y = path()\y
      DeleteElement(path())
    Else
      Break
    EndIf
  ForEver

EndProcedure

;demonstration
If OpenConsole()
  Define.POINT start, finish
  start\x = Random(mazeWidth - 1): start\y = Random(mazeHeight - 1)
  finish\x = Random(mazeWidth - 1): finish\y = Random(mazeHeight - 1)
  NewList Path.POINT()
  solveMaze(maze(), start, finish, path())
  If ListSize(path()) > 0
    PrintN("Solution found for path between (" + Str(start\x) + ", " + Str(start\y) + ") and (" + Str(finish\x) + ", " + Str(finish\y) + ")")
    displayMazePath(maze(), path())
  Else
    PrintN("No solution found for path between (" + Str(start\x) + ", " + Str(start\y) + ") and (" + Str(finish\x) + ", " + Str(finish\y) + ")")
  EndIf

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
