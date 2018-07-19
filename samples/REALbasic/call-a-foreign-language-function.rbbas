  Declare Function CreateFileW Lib "Kernel32" (FileName As WString, DesiredAccess As Integer, ShareMode As Integer, SecurityAttributes As Integer, _
        CreateDisposition As Integer, Flags As Integer, Template As Integer) As Integer
  Declare Function WriteFile Lib "Kernel32" (fHandle As Integer, writeData As Ptr, numOfBytes As Integer, ByRef numOfBytesWritten As Integer, _
        overlapped As Ptr) As Boolean
  Declare Function GetLastError Lib "Kernel32" () As Integer
  Declare Function CloseHandle Lib "kernel32" (hObject As Integer) As Boolean

  Const FILE_SHARE_READ = &h00000001
  Const FILE_SHARE_WRITE = &h00000002
  Const OPEN_EXISTING = 3

  Dim fHandle As Integer = CreateFileW("C:\foo.txt", 0,  FILE_SHARE_READ Or FILE_SHARE_WRITE, 0, OPEN_EXISTING, 0, 0)
  If fHandle > 0 Then
    Dim mb As MemoryBlock = "Hello, World!"
    Dim bytesWritten As Integer
    If Not WriteFile(fHandle, mb, mb.Size, bytesWritten, Nil) Then
      MsgBox("Error Number: " + Str(GetLastError))
    End If
    Call CloseHandle(fHandle)
  Else
    MsgBox("Error Number: " + Str(GetLastError))
  End If
