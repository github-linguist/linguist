Enumeration
  ;indexes for types of offsets from maze coordinates (x,y)
  #visited ;used to index visited(x,y) in a given direction from current maze cell
  #maze    ;used to index maze() in a given direction from current maze cell
  #wall    ;used to index walls in maze() in a given direction from current maze cell
  #numOffsets = #wall
  ;direction indexes
  #dir_ID = 0 ;identity value, produces no changes
  #firstDir
  #dir_N = #firstDir
  #dir_E
  #dir_S
  #dir_W
  #numDirs = #dir_W
EndEnumeration

DataSection
  ;maze(x,y) offsets for visited, maze, & walls for each direction
  Data.i 1, 1,  0,  0, 0, 0 ;ID
  Data.i 1, 0,  0, -1, 0, 0 ;N
  Data.i 2, 1,  1,  0, 1, 0 ;E
  Data.i 1, 2,  0,  1, 0, 1 ;S
  Data.i 0, 1, -1,  0, 0, 0 ;W
  Data.i %00, %01, %10, %01, %10 ;wall values for ID, N, E, S, W
EndDataSection

#cellDWidth = 4

Structure mazeOutput
  vWall.s
  hWall.s
EndStructure


;setup reference values indexed by type and direction from current map cell
Global Dim offset.POINT(#numOffsets, #numDirs)
Define i, j
For i = 0 To #numDirs
  For j = 0 To #numOffsets
    Read.i offset(j, i)\x: Read.i offset(j, i)\y
  Next
Next

Global Dim wallvalue(#numDirs)
For i = 0 To #numDirs: Read.i wallvalue(i): Next


Procedure makeDisplayMazeRow(Array mazeRow.mazeOutput(1), Array maze(2), y)
  ;modify mazeRow() to produce output of 2 strings showing the vertical walls above and horizontal walls across a given maze row
  Protected x, vWall.s, hWall.s
  Protected mazeWidth = ArraySize(maze(), 1), mazeHeight = ArraySize(maze(), 2)

  vWall = "": hWall = ""
  For x = 0 To mazeWidth
    If maze(x, y) & wallvalue(#dir_N): vWall + "+   ": Else: vWall + "+---": EndIf
    If maze(x, y) & wallvalue(#dir_W): hWall + "    ": Else: hWall + "|   ": EndIf
  Next
  mazeRow(0)\vWall = Left(vWall, mazeWidth * #cellDWidth + 1)
  If y <> mazeHeight: mazeRow(0)\hWall = Left(hWall, mazeWidth * #cellDWidth + 1): Else: mazeRow(0)\hWall = "": EndIf
EndProcedure

Procedure displayMaze(Array maze(2))
  Protected x, y, vWall.s, hWall.s, mazeHeight = ArraySize(maze(), 2)
  Protected Dim mazeRow.mazeOutput(0)

  For y = 0 To mazeHeight
    makeDisplayMazeRow(mazeRow(), maze(), y)
    PrintN(mazeRow(0)\vWall): PrintN(mazeRow(0)\hWall)
  Next
EndProcedure

Procedure generateMaze(Array maze(2), mazeWidth, mazeHeight)
  Dim maze(mazeWidth, mazeHeight) ;Each cell specifies walls present above and to the left of it,
                                  ;array includes an extra row and column for the right and bottom walls
  Dim visited(mazeWidth + 1, mazeHeight + 1) ;Each cell represents a cell of the maze, an extra line of cells are included
                                             ;as padding around the representative cells for easy border detection

  Protected i
  ;mark outside border as already visited (off limits)
  For i = 0 To mazeWidth
    visited(i + offset(#visited, #dir_N)\x, 0 + offset(#visited, #dir_N)\y) = #True
    visited(i + offset(#visited, #dir_S)\x, mazeHeight - 1 + offset(#visited, #dir_S)\y) = #True
  Next
  For i = 0 To mazeHeight
    visited(0 + offset(#visited, #dir_W)\x, i + offset(#visited, #dir_W)\y) = #True
    visited(mazeWidth - 1 + offset(#visited, #dir_E)\x, i + offset(#visited, #dir_E)\y) = #True
  Next

  ;generate maze
  Protected x = Random(mazeWidth - 1), y = Random (mazeHeight - 1), cellCount, nextCell
  visited(x + offset(#visited, #dir_ID)\x, y + offset(#visited, #dir_ID)\y) = #True
  PrintN("Maze of size " + Str(mazeWidth) + " x " + Str(mazeHeight) + ", generation started at " + Str(x) + " x " + Str(y))

  NewList stack.POINT()
  Dim unvisited(#numDirs - #firstDir)
  Repeat
    cellCount = 0
    For i = #firstDir To #numDirs
      If Not visited(x + offset(#visited, i)\x, y + offset(#visited, i)\y)
        unvisited(cellCount) = i: cellCount + 1
      EndIf
    Next

    If cellCount
      nextCell = unvisited(Random(cellCount - 1))
      visited(x + offset(#visited, nextCell)\x, y + offset(#visited, nextCell)\y) = #True
      maze(x + offset(#wall, nextCell)\x, y + offset(#wall, nextCell)\y) | wallvalue(nextCell)

      If cellCount > 1
        AddElement(stack())
        stack()\x = x: stack()\y = y
      EndIf
      x + offset(#maze, nextCell)\x: y + offset(#maze, nextCell)\y
    ElseIf ListSize(stack()) > 0
      x = stack()\x: y = stack()\y
      DeleteElement(stack())
    Else
      Break  ;end maze generation
    EndIf
  ForEver

  ; ;mark random entry and exit point
  ; x = Random(mazeWidth - 1): y = Random(mazeHeight - 1)
  ; maze(x, 0) | wallvalue(#dir_N): maze(mazeWidth, y) | wallvalue(#dir_E)
  ProcedureReturn
EndProcedure

If OpenConsole()
  Dim maze(0, 0)
  Define mazeWidth = Random(5) + 7: mazeHeight = Random(5) + 3
  generateMaze(maze(), mazeWidth, mazeHeight)
  displayMaze(maze())

  Print(#CRLF$ + #CRLF$ + "Press ENTER to exit"): Input()
  CloseConsole()
EndIf
