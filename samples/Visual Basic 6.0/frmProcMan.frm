VERSION 5.00
Begin VB.Form frmProcMan 
   Caption         =   "Process Manager"
   ClientHeight    =   6672
   ClientLeft      =   60
   ClientTop       =   -240
   ClientWidth     =   8616
   BeginProperty Font 
      Name            =   "Tahoma"
      Size            =   8.4
      Charset         =   204
      Weight          =   400
      Underline       =   0   'False
      Italic          =   0   'False
      Strikethrough   =   0   'False
   EndProperty
   Icon            =   "frmProcMan.frx":0000
   KeyPreview      =   -1  'True
   LinkTopic       =   "Form1"
   ScaleHeight     =   6672
   ScaleWidth      =   8616
   Begin VB.Frame fraProcessManager 
      Caption         =   "Itty Bitty Process Manager v."
      BeginProperty Font 
         Name            =   "Tahoma"
         Size            =   8.4
         Charset         =   204
         Weight          =   700
         Underline       =   0   'False
         Italic          =   0   'False
         Strikethrough   =   0   'False
      EndProperty
      Height          =   3975
      Left            =   120
      TabIndex        =   0
      Top             =   120
      Width           =   8415
      Begin VB.CommandButton cmdProcManRefresh 
         Caption         =   "Re&fresh"
         Height          =   495
         Left            =   1440
         TabIndex        =   7
         Top             =   3360
         Width           =   1215
      End
      Begin VB.CommandButton cmdProcManBack 
         Cancel          =   -1  'True
         Caption         =   "E&xit"
         Height          =   495
         Left            =   4440
         TabIndex        =   6
         Top             =   3360
         Width           =   1215
      End
      Begin VB.CommandButton cmdProcManRun 
         Caption         =   "&Run..."
         Height          =   495
         Left            =   2760
         TabIndex        =   5
         Top             =   3360
         Width           =   1215
      End
      Begin VB.CommandButton cmdProcManKill 
         Caption         =   "&Kill process"
         Height          =   495
         Left            =   120
         TabIndex        =   4
         Top             =   3360
         Width           =   1215
      End
      Begin VB.ListBox lstProcessManager 
         Height          =   1185
         IntegralHeight  =   0   'False
         ItemData        =   "frmProcMan.frx":1CFA
         Left            =   120
         List            =   "frmProcMan.frx":1CFC
         MultiSelect     =   2  'Extended
         TabIndex        =   3
         Top             =   600
         Width           =   8175
      End
      Begin VB.CheckBox chkProcManShowDLLs 
         Caption         =   "Show &DLLs"
         Height          =   255
         Left            =   3480
         TabIndex        =   2
         Top             =   330
         Width           =   1935
      End
      Begin VB.ListBox lstProcManDLLs 
         Height          =   1140
         IntegralHeight  =   0   'False
         Left            =   120
         TabIndex        =   1
         Top             =   2040
         Visible         =   0   'False
         Width           =   8175
      End
      Begin VB.Label lblConfigInfo 
         AutoSize        =   -1  'True
         Caption         =   "Loaded DLL libraries by selected process:"
         Height          =   195
         Index           =   9
         Left            =   240
         TabIndex        =   10
         Top             =   1800
         Width           =   2955
      End
      Begin VB.Image imgProcManCopy 
         Height          =   288
         Left            =   2400
         Picture         =   "frmProcMan.frx":1CFE
         ToolTipText     =   "Copy process list to clipboard"
         Top             =   216
         Width           =   288
      End
      Begin VB.Label lblProcManDblClick 
         Caption         =   "Double-click a file to view its properties."
         Height          =   390
         Left            =   5760
         TabIndex        =   9
         Top             =   3330
         Width           =   2295
      End
      Begin VB.Label lblConfigInfo 
         AutoSize        =   -1  'True
         Caption         =   "Running processes:"
         Height          =   195
         Index           =   8
         Left            =   240
         TabIndex        =   8
         Top             =   360
         Width           =   1410
      End
      Begin VB.Image imgProcManSave 
         Height          =   288
         Left            =   2880
         Picture         =   "frmProcMan.frx":21C1
         ToolTipText     =   "Save process list to file.."
         Top             =   216
         Width           =   288
      End
   End
   Begin VB.Menu mnuProcMan 
      Caption         =   "ProcessManager Popup Menu"
      Visible         =   0   'False
      Begin VB.Menu mnuProcManKill 
         Caption         =   "Kill process(es)"
      End
      Begin VB.Menu mnuProcManStr1 
         Caption         =   "-"
      End
      Begin VB.Menu mnuProcManCopy 
         Caption         =   "Copy list to clipboard"
      End
      Begin VB.Menu mnuProcManSave 
         Caption         =   "Save list to disk..."
      End
      Begin VB.Menu mnuProcManStr2 
         Caption         =   "-"
      End
      Begin VB.Menu mnuProcManProps 
         Caption         =   "File properties"
      End
   End
End
Attribute VB_Name = "frmProcMan"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
'[frmProcMan.frm]

Option Explicit

'
' Itty Bitty Process Manager by Merijn Bellekom
'

' Check 'frmMain.frm' to change version number

'v1.00 - original release, later added copy to clipboard button
'v1.01 - added label for dlls, keyboard shortcuts
'v1.01.1 - fixed crash bug in form_resize, added version number to frame
'v1.02 - added PID numbers to process list
'v1.03 - fixed killing multiple processes (it works now.. typos suck)
'        also added PauseProcess to the killing subs :D (excludes self)
'        added right-click menu to listboxes
'        fixed a crash bug with the CompanyName property of RAdmin.exe
'v1.04 - processes that fail to be killed are now resumed again
'--
'v1.05 - dll list is updated when browsing process list with keyboard

'Fork by Dragokas

'v1.06 - Replaced Process listing function by Nt version
'v1.07 - Added ability to enum modules of 64-bit processes

'ADS Enum alternatives - see: https://www.cyberforum.ru/win-api/thread1842785.html

'Private Declare Function CloseHandle Lib "kernel32.dll" (ByVal hObject As Long) As Long
'Private Declare Function ShellExecute Lib "shell32.dll" Alias "ShellExecuteW" (ByVal hwnd As Long, ByVal lpOperation As Long, ByVal lpFile As Long, ByVal lpParameters As Long, ByVal lpDirectory As Long, ByVal nShowCmd As Long) As Long
'Private Declare Function SHRunDialog Lib "shell32.dll" Alias "#61" (ByVal hOwner As Long, ByVal Unknown1 As Long, ByVal Unknown2 As Long, ByVal szTitle As String, ByVal szPrompt As String, ByVal uFlags As Long) As Long
'Private Declare Sub ReleaseCapture Lib "user32.dll" ()

'Private Const TH32CS_SNAPPROCESS = &H2
Private Const TH32CS_SNAPNOHEAPS = &H40000000
'Private Const TH32CS_SNAPMODULE = &H8
'Private Const TH32CS_SNAPTHREAD = &H4
'Private Const PROCESS_TERMINATE = &H1
'Private Const PROCESS_QUERY_INFORMATION = 1024
'Private Const PROCESS_QUERY_LIMITED_INFORMATION = &H1000
'Private Const PROCESS_VM_READ = 16
'Private Const THREAD_SUSPEND_RESUME = &H2

'Private Const LB_SETTABSTOPS = &H192
'Private Const WM_NCLBUTTONDOWN = &HA1
'Private Const HTCAPTION = 2

Private lstProcessManagerHasFocus As Boolean
Private lstProcManDLLsHasFocus As Boolean

Public Function ProcManDLLsHasFocus() As Boolean
    ProcManDLLsHasFocus = lstProcManDLLsHasFocus
End Function

Public Sub RefreshProcessList(objList As ListBox)
    Dim hSnap&, uPE32 As PROCESSENTRY32W
    Dim sExeFile$

    hSnap = CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS Or TH32CS_SNAPNOHEAPS, 0)
    
    uPE32.dwSize = Len(uPE32)
    If Process32First(hSnap, uPE32) = 0 Then
        CloseHandle hSnap
        Exit Sub
    End If
    
    objList.Clear
    Do
        sExeFile = StringFromPtrW(VarPtr(uPE32.szExeFile(0)))
        objList.AddItem uPE32.th32ProcessID & vbTab & sExeFile
    Loop Until Process32Next(hSnap, uPE32) = 0
    CloseHandle hSnap
End Sub

Public Sub RefreshProcessListNT(objList As ListBox)
        Dim lNumProcesses As Long, i As Long
        Dim sProcessName As String
        Dim Process() As MY_PROC_ENTRY
        
        lNumProcesses = GetProcesses(Process)
        
        If lNumProcesses Then
        
            For i = 0 To UBound(Process)
        
                sProcessName = Process(i).Path
                
                If Len(Process(i).Path) = 0 Then
                
                    If Not IsDefaultSystemProcess(Process(i).pid, Process(i).Name, Process(i).Path) Then
                        sProcessName = Process(i).Name '& " (cannot get Process Path)"
                    End If
                End If
                
                If Len(sProcessName) <> 0 Then
                    'objList.AddItem Process(i).PID & vbTab & Process(i).SessionID & vbTab & sProcessName
                    objList.AddItem Process(i).pid & vbTab & sProcessName
                End If
            Next
        End If
End Sub

Public Sub RefreshDLLList(lPID&, objList As ListBox)
    objList.Clear
    If lPID = 0 Then Exit Sub
    
    Dim aModules() As String
    Dim i As Long
    
    aModules = EnumModules64(lPID)
    
    If AryItems(aModules) Then
        For i = 0 To UBound(aModules)
            objList.AddItem aModules(i)
        Next
    End If
End Sub

Public Sub SaveProcessList(objProcess As ListBox, objDLL As ListBox, Optional bDoDLLs As Boolean = False)
    Dim sFilename$, i&, sProcess$, sModule$, hFile As Long
    Dim sList As clsStringBuilder
    
    'Save process list to file.., Text files, All files
    sFilename = SaveFileDialog(Translate(166), AppPath(), "processlist.txt", Translate(167) & " (*.txt)|*.txt|" & Translate(168) & " (*.*)|*.*", Me.hwnd)
    If Len(sFilename) = 0 Then Exit Sub
    
    cmdProcManRefresh_Click
    
    Set sList = New clsStringBuilder
    
    'Header
    sList.Append ChrW$(-257)
    sList.AppendLine "Logfile of Itty Bitty Process Manager v." & ProcManVer & " (HJT Fork v." & AppVerString & ")"
    sList.AppendLine
    sList.Append MakeLogHeader()
    sList.AppendLine
    
    '[full path to filename]
    '[file version]
    '[company name]
    sList.AppendLine "[pid]" & vbTab & Translate(186) & vbTab & vbTab & Translate(187) & vbTab & Translate(188)
    
    For i = 0 To objProcess.ListCount - 1
        sProcess = objProcess.List(i)
        sList.AppendLine sProcess & vbTab & vbTab & _
                  GetFilePropVersion(Mid$(sProcess, InStr(sProcess, vbTab) + 1)) & vbTab & _
                  GetFilePropCompany(Mid$(sProcess, InStr(sProcess, vbTab) + 1))
    Next i
    
    If bDoDLLs Then
        Dim arList() As String, j&, lPID&       'Full image. DLLs of ALL processes.
        
        For i = 0 To objProcess.ListCount - 1
            sProcess = objProcess.List(i)
            lPID = CLng(Left$(sProcess, InStr(sProcess, vbTab) - 1))
            sProcess = Mid$(sProcess, InStr(sProcess, vbTab) + 1)
            arList = EnumModules64(lPID)
            'DLLs loaded by process
            sList.AppendLine vbCrLf & vbCrLf & Translate(189) & " [" & lPID & "] " & sProcess & ":" & vbCrLf
            '[full path to filename]
            '[file version]
            '[company name]
            sList.AppendLine Translate(186) & vbTab & vbTab & Translate(187) & vbTab & Translate(188)
            If AryItems(arList) Then
                For j = 0 To UBound(arList)
                    sModule = arList(j)
                    sList.AppendLine sModule & vbTab & vbTab & GetFilePropVersion(sModule) & vbTab & GetFilePropCompany(sModule)
                Next
            End If
            sList.AppendLine
            DoEvents
        Next
    End If
    
    sList.Append "--" & vbCrLf & "End of file"
    
    If OpenW(sFilename, FOR_OVERWRITE_CREATE, hFile, g_FileBackupFlag) Then
        PutW hFile, 1, StrPtr(sList.ToString), sList.Length * 2
        CloseW hFile
    End If
    
    OpenLogFile sFilename
    
    Set sList = Nothing
End Sub

Public Sub CopyProcessList(objProcess As ListBox, objDLL As ListBox, Optional bDoDLLs As Boolean = False)
    Dim i&, sList$, sProcess$, sModule$
    
    'Process list saved on [*DateTime*]
    'Platform
    '[full path to filename]
    '[file version]
    '[company name]
    
    sList = "Logfile of Itty Bitty Process Manager v." & ProcManVer & " (HJT Fork v." & AppVerString & ")" & vbCrLf & vbCrLf & MakeLogHeader() & vbCrLf & _
            "[pid]" & vbTab & Translate(186) & vbTab & vbTab & Translate(187) & vbTab & Translate(188) & vbCrLf
    For i = 0 To objProcess.ListCount - 1
        sProcess = objProcess.List(i)
        sList = sList & sProcess & vbTab & vbTab & _
                GetFilePropVersion(Mid$(sProcess, InStr(sProcess, vbTab) + 1)) & vbTab & _
                GetFilePropCompany(Mid$(sProcess, InStr(sProcess, vbTab) + 1)) & vbCrLf
    Next i
    
    If bDoDLLs Then
        sProcess = objProcess.List(objProcess.ListIndex)
        sProcess = Mid$(sProcess, InStr(sProcess, vbTab) + 1)
        'DLLs loaded by process
        '[full path to filename]
        '[file version]
        '[company name]
        sList = sList & vbCrLf & vbCrLf & Translate(189) & " " & sProcess & ":" & vbCrLf & vbCrLf & _
                Translate(186) & vbTab & vbTab & Translate(187) & vbTab & Translate(188) & vbCrLf
        For i = 0 To objDLL.ListCount - 1
            sModule = objDLL.List(i)
            sList = sList & sModule & vbTab & vbTab & GetFilePropVersion(sModule) & vbTab & GetFilePropCompany(sModule) & vbCrLf
        Next i
    End If
    
    sList = sList & "--" & vbCrLf & "End of file"
    
    ClipboardSetText sList
    If bDoDLLs Then
        'The process list and dll list have been copied to your clipboard.
        MsgBoxW Translate(1650), vbInformation
    Else
        'The process list has been copied to your clipboard.
        MsgBoxW Translate(1651), vbInformation
    End If
End Sub

Private Sub SetListBoxColumns(objListBox As ListBox)
    Dim lTabStop&(1)
    lTabStop(0) = 40
    lTabStop(1) = 0
    SendMessage objListBox.hwnd, LB_SETTABSTOPS, UBound(lTabStop) + 1, lTabStop(0)
End Sub

Private Sub chkProcManShowDLLs_Click()
    lstProcManDLLs.Visible = CBool(chkProcManShowDLLs.Value)
    
    'lstProcessManager.ListIndex = 0
    lstProcessManager_MouseUp 1, 0, 0, 0
    If lstProcessManager.Visible And lstProcessManager.Enabled Then
        lstProcessManager.SetFocus
    End If
    Form_Resize
End Sub

Private Sub cmdProcManBack_Click()
    Unload Me
End Sub

Private Sub cmdProcManKill_Click()
    Dim sMsg$, i&, s$, HasSelectedProcess As Boolean
    sMsg = Translate(179) & vbCrLf
    'sMsg = "Are you sure you want to close the selected processes?" & vbCrLf
    For i = 0 To lstProcessManager.ListCount - 1
        If lstProcessManager.Selected(i) Then
            sMsg = Replace$(sMsg, "[]", vbCrLf & Mid$(lstProcessManager.List(i), InStr(lstProcessManager.List(i), vbTab) + 1))
            HasSelectedProcess = True
        End If
    Next i
    'sMsg = sMsg & vbCrLf & "Any unsaved data in it will be lost."
    If HasSelectedProcess Then
        sMsg = sMsg & vbCrLf & Translate(180)
        If MsgBoxW(sMsg, vbExclamation + vbYesNo) = vbNo Then Exit Sub
    Else
        MsgBoxW Translate(184), vbExclamation
        Exit Sub
    End If
    
    'SetCurrentProcessPrivileges "SeDebugPrivilege"
    
    'pause selected processes
    For i = 0 To lstProcessManager.ListCount - 1
        If lstProcessManager.Selected(i) Then
            s = lstProcessManager.List(i)
            s = Left$(s, InStr(s, vbTab) - 1)
            PauseProcess CLng(s)
        End If
    Next i
    For i = 0 To lstProcessManager.ListCount - 1
        If lstProcessManager.Selected(i) Then
            s = lstProcessManager.List(i)
            s = Left$(s, InStr(s, vbTab) - 1)
            KillProcess CLng(s)
        End If
    Next i
    SleepNoLock 1000
    'resume any processes still alive
    For i = 0 To lstProcessManager.ListCount - 1
        If lstProcessManager.Selected(i) Then
            s = lstProcessManager.List(i)
            s = Left$(s, InStr(s, vbTab) - 1)
            ResumeProcess CLng(s)
        End If
    Next i
    
    cmdProcManRefresh_Click
End Sub

Private Sub cmdProcManRefresh_Click()
    Dim sPID$
    lstProcessManager.Clear
    If bIsWinNT Then
        RefreshProcessListNT lstProcessManager
    Else
        RefreshProcessList lstProcessManager
    End If
    
    lstProcessManager.ListIndex = 0
    
    If lstProcManDLLs.Visible Then
        sPID = lstProcessManager.List(lstProcessManager.ListIndex)
        sPID = Left$(sPID, InStr(sPID, vbTab) - 1)
    
        RefreshDLLList CLng(sPID), lstProcManDLLs
    End If
    'Running processes:
    lblConfigInfo(8).Caption = Translate(171) & " (" & lstProcessManager.ListCount & ")"
    'Loaded DLL libraries by selected process:
    lblConfigInfo(9).Caption = Translate(178) & " (" & lstProcManDLLs.ListCount & ")"
End Sub

Private Sub cmdProcManRun_Click()
    If Not bIsWinNT Then
        SHRunDialog Me.hwnd, 0, 0, Translate(181), Translate(182), 0
        'SHRunDialog Me.hWnd, 0, 0, "Run", "Type the name of a program, folder, document or Internet resource, and Windows will open it for you.", 0
    Else
        SHRunDialog Me.hwnd, 0, 0, StrConv(Translate(181), vbUnicode), StrConv(Translate(182), vbUnicode), 0
        'SHRunDialog Me.hWnd, 0, 0, StrConv("Run", vbUnicode), StrConv("Type the name of a program, folder, document or Internet resource, and Windows will open it for you.", vbUnicode), 0
    End If
    SleepNoLock 1000
    cmdProcManRefresh_Click
End Sub

Private Sub Form_KeyDown(KeyCode As Integer, Shift As Integer)
    If KeyCode = 27 Then Me.Hide
    ProcessHotkey KeyCode, Me
End Sub

Private Sub Form_Load()
    SetAllFontCharset Me, g_FontName, g_FontSize, g_bFontBold
    ReloadLanguage True
    LoadWindowPos Me, SETTINGS_SECTION_PROCMAN
    
    'Process Manager
    Me.Caption = Translate(170)
    cmdProcManRefresh_Click
    'Itty Bitty Process Manager - v
    fraProcessManager.Caption = Translate(160) & ProcManVer 'App.Major & "." & format$(App.Minor, "00") & "." & App.Revision
    SetListBoxColumns lstProcessManager
    AddHorizontalScrollBarToResults lstProcessManager
End Sub

Private Sub Form_MouseDown(Button As Integer, Shift As Integer, X As Single, Y As Single)
'    If Button = 1 Then
'        ReleaseCapture
'        SendMessage Me.hwnd, WM_NCLBUTTONDOWN, HTCAPTION, 0
'    End If
End Sub

Private Sub Form_QueryUnload(Cancel As Integer, UnloadMode As Integer)

    SaveWindowPos Me, SETTINGS_SECTION_PROCMAN

    If UnloadMode = 0 Then 'initiated by user (clicking 'X')
        Cancel = True
        Me.Hide
    End If
End Sub

Private Sub Form_Resize()
    On Error Resume Next
    If Me.WindowState = vbMinimized Then Exit Sub
    fraProcessManager.Width = Me.ScaleWidth - 240
    lstProcessManager.Width = Me.ScaleWidth - 480
    lstProcManDLLs.Width = Me.ScaleWidth - 480
    chkProcManShowDLLs.Left = Me.ScaleWidth - 2200
    imgProcManSave.Left = Me.ScaleWidth - 2700
    imgProcManCopy.Left = Me.ScaleWidth - 3100

    fraProcessManager.Height = Me.ScaleHeight - 125
    If chkProcManShowDLLs.Value = 0 Then
        lstProcessManager.Height = Me.ScaleHeight - 1470
    Else
        lstProcessManager.Height = (Me.ScaleHeight - 1470) / 2 - 120
        lblConfigInfo(9).Top = (Me.ScaleHeight - 1470) / 2 + 480
        lstProcManDLLs.Top = (Me.ScaleHeight - 1470) / 2 + 720
        lstProcManDLLs.Height = Me.ScaleHeight - 1590 - (Me.ScaleHeight - 1470) / 2
    End If
    cmdProcManKill.Top = Me.ScaleHeight - 720
    cmdProcManRefresh.Top = Me.ScaleHeight - 720
    cmdProcManRun.Top = Me.ScaleHeight - 720
    cmdProcManBack.Top = Me.ScaleHeight - 720
    lblProcManDblClick.Top = Me.ScaleHeight - 720
End Sub

'Private Sub fraProcessManager_MouseMove(Button As Integer, Shift As Integer, x As Single, y As Single)
'    ReleaseCapture
'    SendMessage Me.hwnd, WM_NCLBUTTONDOWN, HTCAPTION, 0
'End Sub

Private Sub imgProcManCopy_Click()
    imgProcManCopy.BorderStyle = 1
    DoEvents
    If chkProcManShowDLLs.Value = 1 Then
        CopyProcessList lstProcessManager, lstProcManDLLs, True
    Else
        CopyProcessList lstProcessManager, lstProcManDLLs, False
    End If
    imgProcManCopy.BorderStyle = 0
End Sub

Private Sub imgProcManSave_Click()
    imgProcManSave.BorderStyle = 1
    DoEvents
    If chkProcManShowDLLs.Value = 1 Then
        SaveProcessList lstProcessManager, lstProcManDLLs, True
    Else
        SaveProcessList lstProcessManager, lstProcManDLLs, False
    End If
    imgProcManSave.BorderStyle = 0
End Sub

Private Sub lstProcessManager_Click()
    If lstProcManDLLs.Visible = False Then Exit Sub
    Dim sPID$
    If lstProcessManager.ListIndex = -1 Then lstProcessManager.ListIndex = 0: lstProcessManager.Selected(0) = True
    sPID = lstProcessManager.List(lstProcessManager.ListIndex)
    sPID = Left$(sPID, InStr(sPID, vbTab) - 1)
    
    RefreshDLLList CLng(sPID), lstProcManDLLs
    
    lblConfigInfo(9).Caption = Translate(178) & " (" & lstProcManDLLs.ListCount & ")"
    'lblConfigInfo(9).Caption = "Loaded DLL libraries by selected process: (" & lstProcManDLLs.ListCount & ")"
End Sub

Private Sub lstProcessManager_DblClick()
    Dim s$
    If lstProcessManager.ListIndex = -1 Then
        If lstProcessManager.ListCount <> 0 Then s = lstProcessManager.List(0) Else Exit Sub
    Else
        s = lstProcessManager.List(lstProcessManager.ListIndex)
    End If
    s = Mid$(s, InStr(s, vbTab) + 1)
    ShowFileProperties s, Me.hwnd
End Sub

Private Sub lstProcManDLLs_DblClick()
    Dim s$
    If lstProcManDLLs.ListIndex = -1 Then
        If lstProcManDLLs.ListCount <> 0 Then s = lstProcManDLLs.List(0) Else Exit Sub
    Else
        s = lstProcManDLLs.List(lstProcManDLLs.ListIndex)
    End If
    s = Mid$(s, InStr(s, vbTab) + 1)
    ShowFileProperties s, Me.hwnd
End Sub

Private Sub lstProcManDLLs_KeyDown(KeyCode As Integer, Shift As Integer)
    If KeyCode = 13 Then lstProcManDLLs_DblClick
End Sub

Private Sub lstProcessManager_KeyDown(KeyCode As Integer, Shift As Integer)
    Select Case KeyCode
        Case 13: lstProcessManager_DblClick
        Case 33, 34, 35, 36, 37, 38, 40: lstProcessManager_MouseUp 1, 0, 0, 0
        Case 27: Me.Hide
    End Select
End Sub

Private Sub lstProcessManager_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 1 Then
        If lstProcManDLLs.Visible = False Then Exit Sub
        Dim sPID$
        sPID = lstProcessManager.List(lstProcessManager.ListIndex)
        sPID = Left$(sPID, InStr(sPID, vbTab) - 1)
        
        RefreshDLLList CLng(sPID), lstProcManDLLs
        'Loaded DLL libraries by selected process:
        lblConfigInfo(9).Caption = Translate(178) & " (" & lstProcManDLLs.ListCount & ")"
    ElseIf Button = 2 Then
        PopupMenu mnuProcMan, , , , mnuProcManProps
    End If
End Sub

Private Sub lstProcManDLLs_MouseUp(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Button = 2 Then
        mnuProcManKill.Enabled = False
        PopupMenu mnuProcMan, , , , mnuProcManProps
        mnuProcManKill.Enabled = True
    End If
End Sub

Private Sub mnuProcManCopy_Click()
    imgProcManCopy_Click
End Sub

Private Sub mnuProcManKill_Click()
    cmdProcManKill_Click
End Sub

Private Sub lstProcessManager_LostFocus()
    lstProcessManagerHasFocus = False
End Sub
Private Sub lstProcessManager_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Not lstProcessManagerHasFocus Then
        If GetForegroundWindow() = lstProcessManager.Parent.hwnd Then
            lstProcessManagerHasFocus = True
            If lstProcessManager.Visible And lstProcessManager.Enabled Then
                lstProcessManager.SetFocus
            End If
        End If
    End If
End Sub

Private Sub mnuProcManProps_Click()
    If lstProcessManagerHasFocus Then
        lstProcessManager_DblClick
    Else
        lstProcManDLLs_DblClick
    End If
End Sub

Private Sub mnuProcManSave_Click()
    imgProcManSave_Click
End Sub

Private Sub lstProcManDLLs_LostFocus()
    lstProcManDLLsHasFocus = False
End Sub
Private Sub lstProcManDLLs_MouseMove(Button As Integer, Shift As Integer, X As Single, Y As Single)
    If Not lstProcManDLLsHasFocus Then
        If GetForegroundWindow() = lstProcManDLLs.Parent.hwnd Then
            lstProcManDLLsHasFocus = True
            If lstProcManDLLs.Visible And lstProcManDLLs.Enabled Then
                lstProcManDLLs.SetFocus
            End If
        End If
    End If
End Sub
