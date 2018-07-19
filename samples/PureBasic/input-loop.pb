If OpenConsole()
  ; file based line wise
  If ReadFile(0, "Text.txt")
    While Eof(0) = 0
      Debug ReadString(0)      ; each line until eof
    Wend
    CloseFile(0)
  EndIf

  ; file based byte wise
  If ReadFile(1, "Text.bin")
    While Eof(1) = 0
      Debug ReadByte(1)      ; each byte until eof
    Wend
    CloseFile(1)
  EndIf
EndIf
