Attribute VB_Name = "modFrm"

Option Explicit
Option Compare Binary
Option Base 0

'form move
Public Declare Function SendMessage Lib "User32" Alias "SendMessageA" (ByVal hWND As Long, ByVal wMsg As Long, ByVal wParam As Long, lParam As Any) As Long
Public Declare Function ReleaseCapture Lib "User32" () As Long

'keep on top
Private Declare Function SetWindowPos Lib "User32" (ByVal hWND As Long, ByVal hWndInsertAfter As Long, ByVal x As Long, ByVal y As Long, ByVal cx As Long, ByVal cy As Long, ByVal wFlags As Long) As Long
Private Const HWND_TOPMOST = -1
Private Const HWND_NOTOPMOST = -2
Private Const SWP_NOACTIVATE = &H10
Private Const SWP_NOMOVE = &H2
Private Const SWP_NOSIZE = &H1

'windows
Public Declare Function IsIconic Lib "User32" (ByVal hWND As Long) As Long
Public Declare Function ShowWindow Lib "User32" (ByVal hWND As Long, ByVal nCmdShow As Long) As Long
Public Const SW_MINIMIZE = 6
Public Const SW_SHOWDEFAULT = 10
Public Const SW_SHOWMAXIMIZED = 3
Public Const SW_SHOWMINIMIZED = 2
Public Const SW_SHOWMINNOACTIVE = 7
Public Const SW_SHOWNA = 8
Public Const SW_SHOWNOACTIVATE = 4
Public Const SW_SHOWNORMAL = 1
Public Const SW_SHOW = 5
Public Const SW_RESTORE = 9

Public Sub KeepOnTop(Frm As Form, State As Boolean)
Select Case State
    Case True
        SetWindowPos Frm.hWND, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE Or SWP_NOSIZE Or SWP_NOMOVE
    Case False
        SetWindowPos Frm.hWND, HWND_NOTOPMOST, 0, 0, 0, 0, SWP_NOACTIVATE Or SWP_NOSIZE Or SWP_NOMOVE
End Select
End Sub
