VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} stdSaveHandler 
   Caption         =   "Microsoft Excel"
   ClientHeight    =   1500
   ClientLeft      =   45
   ClientTop       =   390
   ClientWidth     =   6240
   OleObjectBlob   =   "stdSaveHandler.frx":0000
   StartUpPosition =   1  'CenterOwner
End
Attribute VB_Name = "stdSaveHandler"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Private WithEvents pWorkbook As Workbook
Attribute pWorkbook.VB_VarHelpID = -1
Public Event BeforeShow(obj As stdSaveHandler)
Public Event AfterShow(obj As stdSaveHandler)
Public Event WorkbookClose()
Public Event WorkbookCancelSave()
Public Event WorkbookBeforeSave()
Public Event WorkbookAfterSave()

Public Cancel As Boolean
Public Save As Boolean
Private bClosingAlready As Boolean



Public Function Create(wb As Workbook) As stdSaveHandler
  Dim obj As stdSaveHandler
  Set obj = New stdSaveHandler
  obj.Init wb
  Set Create = obj
End Function
Friend Sub Init(wb As Workbook)
  Set pWorkbook = wb
End Sub

'Button events
Private Sub btnCancel_Click()
  Me.Cancel = True
  Me.Hide
End Sub
Private Sub UserForm_QueryClose(Cancel As Integer, CloseMode As Integer)
  Me.Cancel = True
  Me.Hide
End Sub

Private Sub btnDontSave_Click()
  Me.Cancel = False
  Me.Save = False
  Me.Hide
End Sub

Private Sub btnSave_Click()
  Me.Cancel = False
  Me.Save = True
  Me.Hide
End Sub

Private Sub pWorkbook_BeforeClose(Cancel As Boolean)
  If Not bClosingAlready Then
    'Show workbook and raise events either side
    RaiseEvent BeforeShow(Me)
      Me.labelText = "Want to save your changes to '" & ThisWorkbook.Name & "'?"
      Me.Show
    RaiseEvent AfterShow(Me)
    
    'Always cancel main application dialog
    Cancel = True
    
    'If cancelled don't raise WorkbookClose event as the workbook is not closing
    If Me.Cancel Then
      RaiseEvent WorkbookCancelSave
      Exit Sub
    End If
    
    'If me.save is set then save the workbook and call the necessary events
    If Me.Save Then
      RaiseEvent WorkbookBeforeSave
        ThisWorkbook.Save
      RaiseEvent WorkbookAfterSave
    End If
    
    'Raise workbook close event
    RaiseEvent WorkbookClose
    
    'Close workbook without saving
    bClosingAlready = True
    ThisWorkbook.Close False
  End If
End Sub
