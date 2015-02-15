file = %A_WorkingDir%\z.dat
IfExist, %A_WorkingDir%\z.dat
	FileDelete %file%
IfNotEqual ErrorLevel,0, MsgBox Can't delete file "%file%"`nErrorLevel = "%ErrorLevel%"

res := BinWrite(file,"000102030405060708090a0b0c0d0e0f00")
MsgBox ErrorLevel = %ErrorLevel%`nBytes Written = %res%
res := BinRead(file,data)
MsgBox ErrorLevel = %ErrorLevel%`nBytes Read = %res%`nData = "%data%"

res := BinWrite(file,"aa00bb",0,2)
MsgBox ErrorLevel = %ErrorLevel%`nBytes Written = %res%
res := BinRead(file,data)
MsgBox ErrorLevel = %ErrorLevel%`nBytes Read = %res%`nData = "%data%"

res := BinRead(file,data,3,-2)
MsgBox ErrorLevel = %ErrorLevel%`nBytes Read = %res%`nData = "%data%"
ExitApp

/* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BinWrite ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|  - Open binary file
|  - (Over)Write n bytes (n = 0: all)
|  - From offset (offset < 0: counted from end)
|  - Close file
|  data -> file[offset + 0..n-1], rest of file unchanged
|  Return #bytes actually written
*/ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BinWrite(file, data, n=0, offset=0)
{
   ; Open file for WRITE (0x40..), OPEN_ALWAYS (4): creates only if it does not exists
   h := DllCall("CreateFile","str",file,"Uint",0x40000000,"Uint",0,"UInt",0,"UInt",4,"Uint",0,"UInt",0)
   IfEqual h,-1, SetEnv, ErrorLevel, -1
   IfNotEqual ErrorLevel,0,Return,0 ; couldn't create the file

   m = 0                            ; seek to offset
   IfLess offset,0, SetEnv,m,2
   r := DllCall("SetFilePointerEx","Uint",h,"Int64",offset,"UInt *",p,"Int",m)
   IfEqual r,0, SetEnv, ErrorLevel, -3
   IfNotEqual ErrorLevel,0, {
      t = %ErrorLevel%              ; save ErrorLevel to be returned
      DllCall("CloseHandle", "Uint", h)
      ErrorLevel = %t%              ; return seek error
      Return 0
   }

   TotalWritten = 0
   m := Ceil(StrLen(data)/2)
   If (n <= 0 or n > m)
       n := m
   Loop %n%
   {
      StringLeft c, data, 2         ; extract next byte
      StringTrimLeft data, data, 2  ; remove  used byte
      c = 0x%c%                     ; make it number
      result := DllCall("WriteFile","UInt",h,"UChar *",c,"UInt",1,"UInt *",Written,"UInt",0)
      TotalWritten += Written       ; count written
      if (!result or Written < 1 or ErrorLevel)
         break
   }

   IfNotEqual ErrorLevel,0, SetEnv,t,%ErrorLevel%

   h := DllCall("CloseHandle", "Uint", h)
   IfEqual h,-1, SetEnv, ErrorLevel, -2
   IfNotEqual t,,SetEnv, ErrorLevel, %t%

   Return TotalWritten
}

/* ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; BinRead ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
|  - Open binary file
|  - Read n bytes (n = 0: all)
|  - From offset (offset < 0: counted from end)
|  - Close file
|  data (replaced) <- file[offset + 0..n-1]
|  Return #bytes actually read
*/ ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

BinRead(file, ByRef data, n=0, offset=0)
{
   h := DllCall("CreateFile","Str",file,"Uint",0x80000000,"Uint",3,"UInt",0,"UInt",3,"Uint",0,"UInt",0)
   IfEqual h,-1, SetEnv, ErrorLevel, -1
   IfNotEqual ErrorLevel,0,Return,0 ; couldn't open the file

   m = 0                            ; seek to offset
   IfLess offset,0, SetEnv,m,2
   r := DllCall("SetFilePointerEx","Uint",h,"Int64",offset,"UInt *",p,"Int",m)
   IfEqual r,0, SetEnv, ErrorLevel, -3
   IfNotEqual ErrorLevel,0, {
      t = %ErrorLevel%              ; save ErrorLevel to be returned
      DllCall("CloseHandle", "Uint", h)
      ErrorLevel = %t%              ; return seek error
      Return 0
   }

   TotalRead = 0
   data =
   IfEqual n,0, SetEnv n,0xffffffff ; almost infinite

   format = %A_FormatInteger%       ; save original integer format
   SetFormat Integer, Hex           ; for converting bytes to hex

   Loop %n%
   {
      result := DllCall("ReadFile","UInt",h,"UChar *",c,"UInt",1,"UInt *",Read,"UInt",0)
      if (!result or Read < 1 or ErrorLevel)
         break
      TotalRead += Read             ; count read
      c += 0                        ; convert to hex
      StringTrimLeft c, c, 2        ; remove 0x
      c = 0%c%                      ; pad left with 0
      StringRight c, c, 2           ; always 2 digits
      data = %data%%c%              ; append 2 hex digits
   }

   IfNotEqual ErrorLevel,0, SetEnv,t,%ErrorLevel%

   h := DllCall("CloseHandle", "Uint", h)
   IfEqual h,-1, SetEnv, ErrorLevel, -2
   IfNotEqual t,,SetEnv, ErrorLevel, %t%

   SetFormat Integer, %format%      ; restore original format
   Totalread += 0                   ; convert to original format
   Return TotalRead
}
