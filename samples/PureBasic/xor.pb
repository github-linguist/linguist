Procedure.s XorString(String.s, Password.s)
    
    StringLength = Len(String)
    PasswordLength = Len(Password)
  
    For StringIndex = 1 To StringLength
        
        For PasswordIndex = 1 To PasswordLength
            Char.c = Asc(Mid(String, StringIndex, 1)) ! ~Asc(Mid(Password, PasswordIndex, 1))
        Next
        
        Encoded.s = Encoded + Chr(Char)
        
    Next

    ProcedureReturn Encoded
  
EndProcedure

Procedure.b XorFile(Filepath.s, Password.s)
    
    If FileSize(Filepath)
        
        File = OpenFile(#PB_Any, Filepath)
        
        If File
        
            FileLength = Lof(File)
            FileBuffer = AllocateMemory(FileLength)
            PasswordLength = Len(Password)
            
            While Eof(File) = 0
                
                Pointer = Loc(File)
                String.s = ReadString(File)
                
                For StringIndex = 1 To Len(String)
        
                    For PasswordIndex = 1 To PasswordLength
                        Char.c = Asc(Mid(String, StringIndex, 1)) ! ~Asc(Mid(Password, PasswordIndex, 1))
                    Next
                    
                    Encoded.s = Encoded + Chr(Char)
                    
                    FileSeek(File, Pointer)
                    WriteString(File, Encoded)
                
                Next
                
            Wend
            
            ProcedureReturn #True
        
        EndIf
        
    EndIf
    
    ProcedureReturn #False
    
EndProcedure