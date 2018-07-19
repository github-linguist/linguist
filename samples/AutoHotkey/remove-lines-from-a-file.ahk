RemoveLines(filename, startingLine, numOfLines){
       Loop, Read, %filename%
               if ( A_Index < StartingLine )
                       || ( A_Index >= StartingLine + numOfLines )
                               ret .= "`r`n" . A_LoopReadLine
       FileDelete, % FileName
       FileAppend, % SubStr(ret, 3), % FileName
}

SetWorkingDir, % A_ScriptDir
RemoveLines("test.txt", 4, 3)
