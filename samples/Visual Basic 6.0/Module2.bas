Attribute VB_Name = "modMain"
'
' Type library registration tool by Alex Dragokas
'
' v.1.1
'

Option Explicit

Private Declare Function WriteFile Lib "kernel32" (ByVal hFile As Long, ByVal lpBuffer As Long, ByVal nNumberOfBytesToWrite As Long, lpNumberOfBytesWritten As Long, ByVal lpOverlapped As Long) As Long
Private Declare Function GetStdHandle Lib "kernel32" (ByVal nStdHandle As Long) As Long
Private Declare Sub ExitProcess Lib "kernel32" (ByVal uExitCode As Long)
Private Declare Function OemToChar Lib "user32.dll" Alias "OemToCharA" (ByVal lpszScr As String, ByVal lpszDst As String) As Long
Private Declare Function CharToOem Lib "user32.dll" Alias "CharToOemA" (ByVal lpszScr As String, ByVal lpszDst As String) As Long
Private Declare Function LoadTypeLib Lib "OleAut32.dll" (ByVal szFile As Long, pptlib As ITypeLib) As Long
Private Declare Function RegisterTypeLib Lib "OleAut32.dll" (ByVal ptlib As ITypeLib, ByVal szFullPath As Long, ByVal szHelpDir As Long) As Long
Private Declare Function RegisterTypeLibForUser Lib "OleAut32.dll" (ByVal ptlib As ITypeLib, ByVal szFullPath As Long, ByVal szHelpDir As Long) As Long
Private Declare Function GetFileAttributes Lib "kernel32.dll" Alias "GetFileAttributesW" (ByVal lpFileName As Long) As Long
Private Declare Sub OaEnablePerUserTLibRegistration Lib "OleAut32.dll" ()

Const STD_OUTPUT_HANDLE         As Long = -11&
Const STD_ERROR_HANDLE          As Long = -12&
Const INVALID_HANDLE_VALUE      As Long = &HFFFFFFFF
Const FILE_ATTRIBUTE_DIRECTORY  As Long = &H10
Const S_OK                      As Long = 0

Public cOut As Long
Public cErr As Long


Private Sub Main()
    On Error GoTo ErrorHandler
    
    Dim lret        As Long
    Dim FileName    As String
    Dim ExitCode    As Long
    Dim hLib        As Long
    Dim ITL         As ITypeLib
    Dim argv()      As String
    Dim argc        As Long
    Dim i           As Long
    Dim bUseAdmin   As Boolean
    
    cOut = GetStdHandle(STD_OUTPUT_HANDLE)
    cErr = GetStdHandle(STD_ERROR_HANDLE)
    ExitCode = 1
    
    WriteC ""
    
    If Len(Command()) = 0 Then Using: ExitProcess 1
    
    ParseCommandLine Command(), argc, argv
    
    For i = 1 To argc
        Select Case UCase(argv(i))
        
            Case "/ADMIN"
                bUseAdmin = True
                ReLaunch
                
            Case Else
                FileName = UnQuote(argv(i))
        End Select
    Next
    
    WriteC "FILE: " & FileName
    
    lret = GetFileAttributes(StrPtr(FileName))
    
    If lret <> INVALID_HANDLE_VALUE And (0 = (lret And FILE_ATTRIBUTE_DIRECTORY)) Then
        
        lret = LoadTypeLib(StrPtr(FileName), ITL)
        
        If lret = S_OK Then
            
            If bUseAdmin Then
                lret = RegisterTypeLib(ITL, StrPtr(FileName), 0&)
            Else
                lret = RegisterTypeLibForUser(ITL, StrPtr(FileName), 0&) 'by default
            End If
            
            If lret = S_OK Then
                
                ExitCode = 0
            Else
                WriteC TlibErr2Text(lret), cErr
            End If
        Else
            WriteC TlibErr2Text(lret), cErr
        End If
    Else
        WriteC "File is not found.", cErr
    End If
    
    If 0 = ExitCode Then
        WriteC "Success."
    Else
        If IsProcessElevated() And Not bUseAdmin Then
            WriteC "", cErr
            WriteC "WARNING: process launched with administrative privileges.", cErr
            WriteC "It is not guarantied succesful registration in such mode.", cErr
            WriteC "Please, use /admin key, or run this application as non-elevated.", cErr
            WriteC "", cErr
        End If
        WriteC "Failed!", cErr
    End If
    
    'MsgBox "ready"
    ExitProcess ExitCode
    Exit Sub
ErrorHandler:
    WriteC "Error #" & Err.Number & ". LastDll: 0x" & Hex(Err.LastDllError) & ". " & Err.Description, cErr
    ExitProcess 1
End Sub

Private Function TlibErr2Text(lErr As Long)
    Dim sMsg As String
    If lErr = S_OK Then
        TlibErr2Text = "Success."
    Else
        On Error Resume Next
        Err.Raise lErr
        TlibErr2Text = "Error: " & CStr(lErr) & " - " & Err.Description
    End If
End Function

Public Sub WriteC(ByVal txt As String, Optional cHandle As Long)
    Dim dwWritten As Long
    Debug.Print txt
    txt = txt & vbNewLine
    Call CharToOem(txt, txt)
    WriteFile IIf(cHandle = 0, cOut, cHandle), StrPtr(StrConv(txt, vbFromUnicode)), Len(txt), dwWritten, 0&
End Sub

Private Function UnQuote(sStr As String) As String
    If Left$(sStr, 1) = """" And Right$(sStr, 1) = """" And Len(sStr) > 1 Then
        UnQuote = Mid$(sStr, 2, Len(sStr) - 2)
    Else
        UnQuote = sStr
    End If
End Function

Sub Using()
    WriteC "Type library registration tool by Alex Dragokas"
    WriteC ""
    WriteC "Using:"
    WriteC ""
    WriteC "Regtlib.exe [Path\file.tlb] [/admin]"
    WriteC ""
    WriteC "/admin - [optional key], to register tlb under HKLM (system wide) hive."
End Sub

Sub ReLaunch()
    Dim lExitCode As Long
    If Not IsProcessElevated() Then
    
        lExitCode = RunAsAndWait(App.Path & "\" & App.EXEName, Command())
        
        '// TODO: how to return user defined exit code from ShellExecuteEx correctly? ( I don't know )
        
        If 0 = lExitCode Then
            WriteC "Success."
        Else
            WriteC "Exit code: " & lExitCode, cErr
            WriteC "Failed!", cErr
        End If

        ExitProcess lExitCode
    End If
End Sub
