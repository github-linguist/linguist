VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "WorkbookReporter"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = True
''
' DisplayReporter v2.0.0-beta.3
' (c) Tim Hall - https://github.com/VBA-tools/VBA-TDD
'
' Report results to Worksheet
'
' @class DisplayReporter
' @compatibility
'   Platforms: Windows and Mac
'   Applications: Excel-only
' @author tim.hall.engr@gmail.com
' @license MIT (https://opensource.org/licenses/MIT)
' ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ '
Option Explicit

' --------------------------------------------- '
' Constants and Private Variables
' --------------------------------------------- '

Private Const ProgressWidth As Long = 128
Private pSheet As Worksheet
Private pCount As Long
Private pTotal As Long
Private pSuites As Collection

' ============================================= '
' Public Methods
' ============================================= '

''
' Connect the display runner to a Worksheet to output results
'
' The given Worksheet should have names for:
' - "Progress" (Shape with width)
' - "ProgressBorder" (Shape)
' - "Result" (Cell) - Cell to output overall result
' - "Output" (Cell) - First cell to output results
'
' @method ConnectTo
' @param {Worksheet} Sheet
''
Public Sub ConnectTo(Sheet As Worksheet)
    Set pSheet = Sheet
End Sub

''
' Call this at the beginning of a test run to reset the worksheet
' (pass overall number of test suites that will be run to display progress)
'
' @method Start
' @param {Long} [NumSuites = 0]
''
Public Sub Start(Optional NumSuites As Long = 0)
    pCount = 0
    pTotal = NumSuites

    ClearResults
    ShowProgress
    DisplayResult "Running"
End Sub

''
' Output the given suite
'
' @method Output
' @param {TestSuite} Suite
''
Public Sub Output(Suite As TestSuite)
    pCount = pCount + 1
    pSuites.Add Suite
    
    ShowProgress
    DisplayResults
End Sub

''
' After outputing all suites, display overall result
'
' @method Done
''
Public Sub Done()
    Dim Failed As Boolean
    Dim Suite As TestSuite
    For Each Suite In pSuites
        If Suite.Result = TestResultType.Fail Then
            Failed = True
            Exit For
        End If
    Next Suite
    
    DisplayResult IIf(Failed, "FAIL", "PASS")
End Sub

' ============================================= '
' Private Functions
' ============================================= '

Private Sub ShowProgress()
    If pTotal <= 0 Then
        HideProgress
        Exit Sub
    End If

    Dim Percent As Double
    Percent = pCount / pTotal
    
    If Percent > 1 Then
        Debug.Print "WARNING: DisplayRunner has output more suites than specified in Start"
        Percent = 1
    End If

    pSheet.Shapes("Progress").Width = ProgressWidth * Percent
    pSheet.Shapes("Progress").Visible = True
    pSheet.Shapes("ProgressBorder").Visible = True
End Sub

Private Sub HideProgress()
    pSheet.Shapes("Progress").Visible = False
    pSheet.Shapes("ProgressBorder").Visible = False
End Sub

Private Sub DisplayResult(Value As String)
    With pSheet.Range("Result")
        .Font.Size = IIf(Value = "Running", 12, 14)
        .Value = Value
    End With
End Sub

Private Sub ClearResults()
    Dim StartRow As Long
    Dim StartColumn As Long
    StartRow = pSheet.Range("Output").Row
    StartColumn = pSheet.Range("Output").Column
    
    Dim lastrow As Long
    lastrow = StartRow
    Do While pSheet.Cells(lastrow + 1, StartColumn).Value <> ""
        lastrow = lastrow + 1
    Loop
    
    With pSheet.Range(pSheet.Cells(StartRow, StartColumn), pSheet.Cells(lastrow, StartColumn + 1))
        .Value = ""
        .Font.Bold = False
        .Borders(xlInsideHorizontal).LineStyle = xlNone
    End With
End Sub

Private Sub DisplayResults()
    Dim Rows As New Collection
    Dim Dividers As New Collection
    Dim Headings As New Collection
    
    Dim Suite As TestSuite
    Dim Test As TestCase
    Dim Failure As Variant
    
    For Each Suite In pSuites
        If Rows.Count > 0 Then
            Dividers.Add Rows.Count
        End If

        If Suite.Description <> "" Then
            Headings.Add Rows.Count
            Rows.Add Array(Suite.Description, ResultTypeToString(Suite.Result))
        End If

        For Each Test In Suite.Tests
            If Test.Result <> TestResultType.Skipped Then
                Rows.Add Array(Test.Name, ResultTypeToString(Test.Result))
    
                For Each Failure In Test.Failures
                    Rows.Add Array("  " & Failure, "")
                Next Failure
            End If
        Next Test
    Next Suite
    
    Dim OutputValues() As String
    Dim Row As Variant
    Dim i As Long
    ReDim OutputValues(Rows.Count - 1, 1)
    i = 0
    For Each Row In Rows
        OutputValues(i, 0) = Row(0)
        OutputValues(i, 1) = Row(1)
        i = i + 1
    Next Row
    
    Dim StartRow As Long
    Dim StartColumn As Long
    StartRow = pSheet.Range("Output").Row
    StartColumn = pSheet.Range("Output").Column

    pSheet.Range(pSheet.Cells(StartRow, StartColumn), pSheet.Cells(StartRow + Rows.Count - 1, StartColumn + 1)).Value = OutputValues
    
    Dim Divider As Variant
    For Each Divider In Dividers
        With pSheet.Range(pSheet.Cells(StartRow + Divider, StartColumn), pSheet.Cells(StartRow + Divider, StartColumn + 1)).Borders(xlEdgeTop)
            .LineStyle = xlContinuous
            .Color = VBA.RGB(191, 191, 191)
            .Weight = xlThin
        End With
    Next Divider
    
    Dim Heading As Variant
    For Each Heading In Headings
        pSheet.Cells(StartRow + Heading, StartColumn).Font.Bold = True
    Next Heading
End Sub

Private Function ResultTypeToString(ResultType As TestResultType) As String
    Select Case ResultType
    Case TestResultType.Pass
        ResultTypeToString = "Pass"
    Case TestResultType.Fail
        ResultTypeToString = "Fail"
    Case TestResultType.Pending
        ResultTypeToString = "Pending"
    End Select
End Function

Private Sub Class_Initialize()
    Set pSuites = New Collection
End Sub
