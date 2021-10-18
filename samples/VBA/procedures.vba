' ------------------------------------------------------------------------------
' Settings
' ------------------------------------------------------------------------------
Option Explicit
' ------------------------------------------------------------------------------
' Procedures
' ------------------------------------------------------------------------------
Public Sub consolidate()
  ' Variables
  Dim btmRow As Long
  Dim check As String
  Dim colOffset As Long
  Dim column As Long
  Dim counter As Long
  Dim dialog As Integer
  Dim file As Long
  Dim filename As String
  Dim files() As String
  Dim fso As New FileSystemObject
  Dim header As Long
  Dim leftCol As Long
  Dim name As String
  Dim namecheck As String
  Dim newfile As String
  Dim path As String
  Dim rightCol As Long
  Dim row As Long
  Dim rowOffset As Long
  Dim source As Range
  Dim target As Range
  Dim topRow As Long
  Dim twb As Workbook
  Dim tws As Worksheet
  Dim version As Integer
  Dim wb As Workbook
  Dim ws As Worksheet
  ' Workbook variables
  Set twb       = Application.ThisWorkbook                                      ' Select workbook
  Let filename  = fso.GetBaseName(twb.FullName)                                 ' Get file fullname
  Set tws       = twb.Sheets(1)                                                 ' Select worksheet
  Let header    = 1                                                             ' Header size
  Let rowOffset = 0                                                             ' Row offset
  Let topRow    = 1 + rowOffset + header                                        ' First row with data
  Let colOffset = 1                                                             ' Column offset (modify according to use cases)
  Let leftCol   = 1 + colOffset                                                 ' First column with data
  ' Start
  GoTo start
quit:
  Let dialog = MsgBox( _
    prompt:="Are you sure you want to quit?", _
    Buttons:=vbExclamation + vbYesNo, _
    Title:=filename)                                                            ' Quit validation message
  If dialog = vbYes Then                                                        ' If user confirms
    twb.Close SaveChanges:=False                                                ' Close the workbook
    If Application.Workbooks.Count = 0 Then                                     ' If only this workbook was open
      Application.quit                                                          ' Quit the application
    End If
  ElseIf dialog = vbNo Then                                                     ' Else
    GoTo start                                                                  ' Go back to start
  End If
start:
  ' User interaction
  Let dialog = MsgBox( _
    prompt:="Are the files in the same folder as this document?", _
    Buttons:=vbQuestion + vbYesNo, _
    Title:=filename)                                                            ' Ask if files are in the current folder
  If dialog = vbYes Then                                                        ' If it is true
    Let path = twb.path                                                         ' Set path to current repository
  ElseIf dialog = vbNo Then                                                     ' If not
    If path = "False" Then Let path = ""                                        ' Reset path if action has been cancelled
    Let path = Application.InputBox( _
      prompt:="Enter the path to the repository containing the files to consolidate:", _
      Title:=filename, _
      Default:=path, _
      Type:=2)                                                                  ' Input source repository path
    ' Check input
    If StrPtr(path) = 0 Or path = "False" Then                                  ' If user cancels action
      GoTo quit                                                                 ' Quit application
    ElseIf path = "" Then                                                       ' If user provided no value
      Let dialog = MsgBox( _
        prompt:="Please provide a valid path.", _
        Buttons:=vbExclamation + vbRetryCancel, _
        Title:=filename)                                                        ' Throw error message
      If dialog = vbRetry Then                                                  ' If user wants to retry
        GoTo start                                                              ' Go back to the beginning
      Else: GoTo quit                                                           ' Else close application
      End If
    End If
  End If
  If Right(path, 1) <> "\" Then                                                 ' If closing backslash is missing
    Let path = path & "\"                                                       ' Add it to the end of the path
  End If
  ' Check if files are found in the repository
  Let check = Dir(path & "*.xl*")                                               ' Check for Excel files and store its name
  If (check = "") Then                                                          ' If no Excel files are found
    Let dialog = MsgBox( _
      prompt:="No files were found.", _
      Buttons:=vbExclamation + vbRetryCancel, _
      Title:=filename)                                                          ' Throw error message
    If dialog = vbRetry Then                                                    ' If user wants to retry
      GoTo start                                                                ' Go back to the beginning
    Else: GoTo quit                                                             ' Else close application
    End If
  End If
version:
  ' Version check
  Let dialog = MsgBox( _
    prompt:="The file will be generated for the year " & Year(Date) & "." & Chr(13) & Chr(13) & "Is this the correct date?", _
    Buttons:=vbQuestion + vbYesNo, _
    Title:=filename)                                                            ' Ask if year is current
  If dialog = vbNo Then                                                         ' If version is not valid
    Let version = Application.InputBox( _
      prompt:="Please enter the correct year:", _
      Title:=filename, _
      Type:=1)                                                                  ' Ask for correct year
    If version = 0 Then                                                         ' If user cancels
      GoTo version                                                              ' Go back to previous step
    End If
  Else
    Let version = Year(Date)                                                    ' Version (year)
  End If
  Let tws.name  = version                                                       ' Rename worksheet
  Let newfile   = filename & " - " & version                                    ' Add version to filename
  ' Iterate on files list
  Let counter = 0                                                               ' Initialise counter
  Do While check <> ""                                                          ' As long are files are found
    Let name = Split(check, ".xl")(0)                                           ' Store workbook name
    If name <> filename And name <> newfile Then                                ' Skip current workbook or consolidated one
      If InStr(name, CStr(version)) > 0 Then                                    ' If files are from the corresponding year
        Let counter = counter + 1                                               ' Increment counter
        ReDim Preserve files(counter)                                           ' Redimension array
        Let files(counter) = check                                              ' Assign document
      End If
    End If
    Let check = Dir()                                                           ' Get next entry
  Loop                                                                          ' Iterate
  If counter = 0 Then                                                           ' If no source files are found for the corresponding year
    Let dialog = MsgBox( _
      prompt:="No Excel files were found for the year " & version & ".", _
      Buttons:=vbExclamation + vbRetryCancel, _
      Title:=filename)                                                          ' Throw error message
    If dialog = vbRetry Then                                                    ' If user wants to retry
      GoTo start                                                                ' Go back to the beginning
    Else: GoTo quit                                                             ' Else close application
    End If
  Else
    ' Settings
    Application.Calculation     = xlCalculationManual                           ' Disable auto-processing
    Application.DisplayAlerts   = False                                         ' Disable alerts
    Application.EnableEvents    = False                                         ' Disable events
    Application.ScreenUpdating  = False                                         ' Disable display updates
    ' Import data
    Let dialog = MsgBox( _
      prompt:="Excel will now consolidate each files found in the repository " & path & " for the year " & version & "." & Chr(13) & Chr(13) & "Please wait until the process is complete.", _
      Buttons:=vbInformation + vbOKCancel, _
      Title:=filename)                                                          ' Start message
    If dialog = vbCancel Then GoTo quit                                         ' If user cancels, go back to quit
    window.Show                                                                 ' Display progress bar
    progress (5)                                                                ' Update progress bar
    ' Import headers
    Set wb        = Workbooks.Open(path & files(1))                             ' Select and open first workbook
    Set ws        = wb.Worksheets(1)                                            ' Select worksheet
    Let column    = getLastColIndex(ws, 1 + rowOffset)                          ' Get column index
    Let topRow    = 1                                                           ' Target first row by default (adjust if needed)
    Let leftCol   = 1                                                           ' Target first column by default (adjust if needed)
    Let rightCol  = column - colOffset                                          ' Adjust for column offset
    Set source    = ws.Range(ws.Cells(1 + rowOffset, leftCol + colOffset), ws.Cells(header + rowOffset, column))  ' Select header
    Set target    = tws.Range(tws.Cells(topRow, leftCol), tws.Cells(header, rightCol))  ' Target cell
    Call copyData(source, target)                                               ' Copy values
    wb.Close SaveChanges:=False                                                 ' Exit source workbook
    Let topRow    = topRow + header                                             ' Reposition row index
    progress (10)                                                               ' Update progress bar
    ' Import values
    For file = LBound(files) To UBound(files)                                   ' Iterate over files list
      Set wb = Nothing                                                          ' Reset pointer
      On Error Resume Next
      Set wb = Workbooks.Open(path & files(file + 1))                           ' Select and open source workbook
      On Error GoTo 0
      If Not wb Is Nothing Then                                                 ' Check that workbook is selected
        On Error Resume Next
        Let namecheck = fso.GetBaseName(wb.FullName)                            ' Get workbook name
        If (namecheck <> filename And namecheck <> newfile) Then                ' Skip current workbook or consolidated one
          Set ws = wb.Worksheets(1)                                             ' Select worksheet
          Let column = getLastColIndex(ws, 1 + rowOffset)                       ' Get column index
          Let rightCol = column - colOffset                                     ' Adjust for column offset
          Let row = getLastRowIndex(ws, 1 + colOffset)                          ' Get row index
          ' Check if file is not empty
          If (row > 1) Then                                                     ' If data is found
            Let btmRow = topRow - 1 - header + row - rowOffset                  ' Adjust for row offsets due to header and/or previous data
            Set source = ws.Range(ws.Cells(1 + header + rowOffset, leftCol + colOffset), ws.Cells(row, column)) ' Source range
            Set target = tws.Range(tws.Cells(topRow, leftCol), tws.Cells(btmRow, rightCol)) ' Target cell
            Call copyData(source, target)                                       ' Copy values
            Let topRow = btmRow + 1                                             ' Reposition top row
            End If
        End If
        wb.Close SaveChanges:=False                                             ' Exit source workbook
      End If
      progress (Round((1 + file) / counter * 80) + 10)                          ' Update progress bar
    Next file                                                                   ' Next file
    progress (90)                                                               ' Update progress bar
    tws.Columns.AutoFit                                                         ' Resize columns width
    ' Let newfile = path & newfile                                                ' Add path to filename
    twb.SaveAs filename:=path & newfile, FileFormat:=xlOpenXMLWorkbook            ' Save as new workbook
    ' Reset settings
    Application.Calculation     = xlCalculationAutomatic
    Application.DisplayAlerts   = True
    Application.EnableEvents    = True
    Application.ScreenUpdating  = True
    ' End
    twb.Save                                                                    ' Save workbook
    window.Hide                                                                 ' Hide progress bar
    Let dialog = MsgBox( _
      prompt:="Files consolidation done." & Chr(13) & Chr(13) & "The file " & newfile & ".xlsx has been generated at location " & path & ".", _
      Buttons:=vbInformation + vbOKOnly, _
      Title:=filename)                                                          ' Notify user of end of process
  End If
End Sub
