
Structure Memory_Operation
  Src_Offset.q
  Src_Size.q
  
  Dst_Offset.q
  Dst_Size.q
  
  Copy_Size.q
EndStructure

; #### Cuts the Offset's / Sizes of the memory operation to prevent memory violations
Procedure Memory_Operation_Check(*Memory_Operation.Memory_Operation)
  Protected Temp.q
  
  If *Memory_Operation\Src_Offset < 0
    *Memory_Operation\Copy_Size  + *Memory_Operation\Src_Offset
    *Memory_Operation\Dst_Offset - *Memory_Operation\Src_Offset
    *Memory_Operation\Src_Offset - *Memory_Operation\Src_Offset
  EndIf
  
  If *Memory_Operation\Dst_Offset < 0
    *Memory_Operation\Copy_Size  + *Memory_Operation\Dst_Offset
    *Memory_Operation\Src_Offset - *Memory_Operation\Dst_Offset
    *Memory_Operation\Dst_Offset - *Memory_Operation\Dst_Offset
  EndIf
  
  Temp = *Memory_Operation\Src_Size - *Memory_Operation\Src_Offset
  If *Memory_Operation\Copy_Size > Temp
    *Memory_Operation\Copy_Size = Temp
  EndIf
  
  Temp = *Memory_Operation\Dst_Size - *Memory_Operation\Dst_Offset
  If *Memory_Operation\Copy_Size > Temp
    *Memory_Operation\Copy_Size = Temp
  EndIf
  
  If *Memory_Operation\Copy_Size < 0
    *Memory_Operation\Copy_Size = 0
  EndIf
  
  ProcedureReturn #True
EndProcedure

; #### Fills a *Destination with a specified amount of data.
; #### It cuts everything, to prevent memory violations
Procedure Memory_Range_Fill(Ascii.a, Fill_Size.q, *Dst, Dst_Offset.q, Dst_Size.q=-1)
  Protected Temp.q
  
  If Not *Dst
    ProcedureReturn #False
  EndIf
  
  If Dst_Size = -1
    Dst_Size.q = MemorySize(*Dst)
  EndIf
  
  If Dst_Offset < 0
    Fill_Size  + Dst_Offset
    Dst_Offset - Dst_Offset
  EndIf
  
  Temp = Dst_Size - Dst_Offset
  If Fill_Size > Temp
    Fill_Size = Temp
  EndIf
  
  If Fill_Size > 0
    FillMemory(*Dst+Dst_Offset, Fill_Size, Ascii)
  EndIf
  
  ProcedureReturn #True
EndProcedure

; #### Copies a specified amount of data (Copy_Size) from the source to the destination.
; #### It cuts everything, to prevent memory violations
Procedure Memory_Range_Copy(*Src, Src_Offset.q, *Dst, Dst_Offset.q, Copy_Size.q, Src_Size.q=-1, Dst_Size.q=-1)
  Protected Temp.q
  If Not *Src
    ProcedureReturn #False
  EndIf
  
  If Not *Dst
    ProcedureReturn #False
  EndIf
  
  If Src_Size = -1
    Src_Size.q = MemorySize(*Src)
  EndIf
  If Dst_Size = -1
    Dst_Size.q = MemorySize(*Dst)
  EndIf
  
  If Src_Offset < 0
    Copy_Size  + Src_Offset
    Dst_Offset - Src_Offset
    Src_Offset - Src_Offset
  EndIf
  
  If Dst_Offset < 0
    Copy_Size  + Dst_Offset
    Src_Offset - Dst_Offset
    Dst_Offset - Dst_Offset
  EndIf
  
  Temp = Src_Size - Src_Offset
  If Copy_Size > Temp
    Copy_Size = Temp
  EndIf
  
  Temp = Dst_Size - Dst_Offset
  If Copy_Size > Temp
    Copy_Size = Temp
  EndIf
  
  If Copy_Size > 0
    CopyMemory(*Src+Src_Offset, *Dst+Dst_Offset, Copy_Size)
  EndIf
  
  ProcedureReturn #True
EndProcedure

; #### Copies (MoveMemory) a specified amount of data (Copy_Size) from the source to the destination.
; #### It cuts everything, to prevent memory violations
Procedure Memory_Range_Move(*Src, Src_Offset.q, *Dst, Dst_Offset.q, Copy_Size.q, Src_Size.q=-1, Dst_Size.q=-1)
  Protected Temp.q
  If Not *Src
    ProcedureReturn #False
  EndIf
  
  If Not *Dst
    ProcedureReturn #False
  EndIf
  
  If Src_Size = -1
    Src_Size.q = MemorySize(*Src)
  EndIf
  If Dst_Size = -1
    Dst_Size.q = MemorySize(*Dst)
  EndIf
  
  If Src_Offset < 0
    Copy_Size  + Src_Offset
    Dst_Offset - Src_Offset
    Src_Offset - Src_Offset
  EndIf
  
  If Dst_Offset < 0
    Copy_Size  + Dst_Offset
    Src_Offset - Dst_Offset
    Dst_Offset - Dst_Offset
  EndIf
  
  Temp = Src_Size - Src_Offset
  If Copy_Size > Temp
    Copy_Size = Temp
  EndIf
  
  Temp = Dst_Size - Dst_Offset
  If Copy_Size > Temp
    Copy_Size = Temp
  EndIf
  
  If Copy_Size > 0
    MoveMemory(*Src+Src_Offset, *Dst+Dst_Offset, Copy_Size)
  EndIf
  
  ProcedureReturn #True
EndProcedure

; #### Mirrors the memory, usable for little/big endian switching
Procedure Memory_Mirror(*Memory, Memory_Size)
  Protected Elements, i
  Protected Temp.a, *A.Ascii, *B.Ascii
  
  If Not *Memory
    ProcedureReturn #False
  EndIf
  
  If Memory_Size < 1
    ProcedureReturn #True
  EndIf
  
  Elements = Memory_Size/2
  *A = *Memory
  *B = *Memory + Memory_Size - 1
  
  For i = 0 To Elements - 1
    Temp = *A\a
    *A\a = *B\a
    *B\a = Temp
    *A + 1
    *B - 1
  Next
  
  ProcedureReturn #True
EndProcedure
; IDE Options = PureBasic 5.31 (Windows - x64)
; CursorPosition = 190
; FirstLine = 177
; Folding = -
; EnableXP
; DisableDebugger
