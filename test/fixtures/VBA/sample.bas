Attribute VB_Name = "Utilities"
Option Explicit

Sub EditExcelRange()
    Dim ws As Worksheet
    Set ws = ThisWorkbook.Sheets("Sheet1")
    
    ' Edit a range in the worksheet
    ws.Range("A1").Value = "Hello, World!"
    ws.Range("A2").Value = 42
    ws.Range("A3").Value = Date
    
    ' Format the range
    With ws.Range("A1:A3")
        .Font.Bold = True
        .Interior.Color = RGB(255, 255, 0) ' Yellow background
    End With
    
    MsgBox "Range edited successfully!"
End Sub