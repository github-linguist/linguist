VERSION 5.00
Begin {C62A69F0-16DC-11CE-9E98-00AA00574A4F} frmConvert 
   OleObjectBlob   =   "frmConvert.frx":0000
   Caption         =   "Excel2LaTeX"
   ClientHeight    =   7440
   ClientLeft      =   45
   ClientTop       =   330
   ClientWidth     =   12075
   StartUpPosition =   1  'CenterOwner
   TypeInfoVer     =   109
End
Attribute VB_Name = "frmConvert"
Attribute VB_Base = "0{86998920-9B42-405F-8BA3-9B10214C86E8}{A87D0BBE-8D68-4653-9A87-9251ECDE6A1F}"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = False
Attribute VB_TemplateDerived = False
Attribute VB_Customizable = False
Option Explicit

Implements IView

Private mController As IController
Attribute mController.VB_VarHelpID = -1
Private WithEvents mControllerEvents As IControllerEvents
Attribute mControllerEvents.VB_VarHelpID = -1

Private mModel As IModel
Private WithEvents mModelEvents As IModelEvents
Attribute mModelEvents.VB_VarHelpID = -1

Private mStorage As IStorage
Private WithEvents mStorageEvents As IStorageEvents
Attribute mStorageEvents.VB_VarHelpID = -1

Private WithEvents mActiveWkSheet As Worksheet
Attribute mActiveWkSheet.VB_VarHelpID = -1

Private mbIgnoreControlEvents As Boolean

'
' IView implementation
'
Private Property Get IView_Controller() As IController
    Set IView_Controller = mController
End Property
Private Property Set IView_Controller(ByVal pController As IController)
    Set mController = pController
    Set mControllerEvents = pController.Events
End Property

Private Property Get IView_Storage() As IStorage
    Set IView_Storage = mStorage
End Property
Private Property Set IView_Storage(ByVal pStorage As IStorage)
    Set mStorage = pStorage
    Set mStorageEvents = pStorage.Events
    LoadStoredTablesList
End Property

Private Sub IView_Show(ByVal Modal As FormShowConstants)
    Me.Show Modal
End Sub


'
' Form implementation
'
Private Function SafeRangePrecedents(ByVal pRange As Range) As Range
    On Error Resume Next
    Set SafeRangePrecedents = pRange.Precedents
End Function

Private Function UnionOfRangeAndItsPrecedents(ByVal pRange As Range) As Range
    Dim pPrecedents As Range
    Set pPrecedents = SafeRangePrecedents(pRange)
    
    If pPrecedents Is Nothing Then
        Set UnionOfRangeAndItsPrecedents = pRange
    Else
        Set UnionOfRangeAndItsPrecedents = Union(pRange, pPrecedents)
    End If
End Function


Private Sub AutoApplyBox_Click()
    ApplyButton.Enabled = Not AutoApplyBox.Value
    If AutoApplyBox.Value Then ApplyButton_Click
End Sub

Private Sub chkBooktabs_Click()
    If AutoApplyBox.Value Then UpdateOptions
End Sub

Private Sub chkConvertDollar_Click()
    If AutoApplyBox.Value Then UpdateOptions
End Sub

Private Sub chkTableFloat_Click()
    If AutoApplyBox.Value Then UpdateOptions
End Sub

Private Sub ApplyButton_Click()
    UpdateOptions
    mModel.CellWidth = txtCellSize
    mModel.Indent = txtIndent
End Sub

Private Sub lvwStoredTables_Change()
    Dim bSelected As Boolean
    bSelected = (lvwStoredTables.ListIndex >= 0)
    cmdLoad.Enabled = bSelected
    cmdDelete.Enabled = bSelected
    cmdOverwrite.Enabled = bSelected
End Sub

Private Sub lvwStoredTables_DblClick(ByVal Cancel As MSForms.ReturnBoolean)
    If cmdLoad.Enabled Then cmdLoad_Click
End Sub

Private Sub lvwStoredTables_KeyDown(ByVal KeyCode As MSForms.ReturnInteger, ByVal Shift As Integer)
    Select Case KeyCode
    Case 46 ' delete
        cmdDelete_Click
    End Select
End Sub

Private Sub mActiveWkSheet_Change(ByVal Target As Range)
    On Error GoTo errfail
    If Not Me.Visible Then Exit Sub
    If Not Intersect(Target, UnionOfRangeAndItsPrecedents(mModel.Range)) Is Nothing Then
        ConvertSelection
    End If
errfail:
End Sub

Private Sub mControllerEvents_ModelChanged()
    Dim pModel As IModel
    Set pModel = mController.Model
    
    Set mModel = pModel
    Set mModelEvents = pModel.Events
    InitFromModel mModel
    
    Set mActiveWkSheet = Nothing
    If Not mModel.Range Is Nothing Then Set mActiveWkSheet = mModel.Range.Worksheet
End Sub

Private Sub mModelEvents_Changed()
    If mbIgnoreControlEvents Then Exit Sub
    SetResult mModel.GetConversionResult
End Sub

Private Sub mStorageEvents_Changed()
    LoadStoredTablesList
End Sub

Private Sub SetResult(ByVal sResult As String)
    #If Mac Then
    txtResult.Locked = False
    #End If
    
    txtResult.Text = sResult
    
    #If Mac Then
    txtResult.Locked = True
    #End If
End Sub

Private Sub ConvertSelection()
    If mbIgnoreControlEvents Then Exit Sub
    SetResult mModel.GetConversionResult
    txtResult.SetFocus
End Sub

Public Sub InitModel(ByVal pModel As IModel)
    With pModel
        .CellWidth = Val(Me.txtCellSize)
        .Options = Me.GetOptions()
        .Indent = Val(Me.txtIndent)
        .FileName = Me.txtFilename
    End With
End Sub

Public Sub InitFromModel(ByVal pModel As IModel)
    mbIgnoreControlEvents = True
    With pModel
        Me.txtCellSize = .CellWidth
        Me.SetOptions (.Options)
        Me.txtIndent = .Indent
        Me.txtFilename = .FileName
        Me.cmdSelection.Caption = .RangeAddress
    End With
    mbIgnoreControlEvents = False
    ConvertSelection
End Sub

Private Sub LoadStoredTablesList()
    lvwStoredTables.Clear
    
    Dim pModel As IModel
    For Each pModel In mStorage.GetItems
        lvwStoredTables.AddItem pModel.Description
    Next
End Sub

Function GetOptions() As x2lOptions
    If chkBooktabs.Value Then GetOptions = GetOptions Or x2lBooktabs
    If chkConvertDollar.Value Then GetOptions = GetOptions Or x2lConvertMathChars
    If chkTableFloat.Value Then GetOptions = GetOptions Or x2lCreateTableEnvironment
End Function
Sub SetOptions(ByVal Options As x2lOptions)
    chkBooktabs.Value = (Options And x2lBooktabs) <> 0
    chkConvertDollar.Value = (Options And x2lConvertMathChars) <> 0
    chkTableFloat.Value = (Options And x2lCreateTableEnvironment) <> 0
End Sub

Private Sub UpdateOptions()
    mModel.Options = GetOptions()
End Sub

Private Sub cmdBrowse_Click()
    Dim sFileName
    sFileName = Application.GetSaveAsFilename(mModel.AbsoluteFileName, "TeX documents (*.tex), *.tex")
    If sFileName <> False Then
        txtFilename = sFileName
    End If
End Sub

Private Sub cmdCancel_Click()
  Hide
End Sub



Private Sub cmdCopy_Click()
    #If VBA7 And Win32 Then
        If Not Win32_SetClipBoard(txtResult) Then Exit Sub
    #Else
        Dim dataObj As New DataObject
        dataObj.SetText txtResult
        dataObj.PutInClipboard
    #End If
    Hide
End Sub

Private Sub cmdSave_Click()
    SaveConversionResultToFile mModel
    Hide
End Sub




Private Sub cmdStore_Click()
    mStorage.Add mModel
    lvwStoredTables.ListIndex = lvwStoredTables.ListCount - 1
End Sub

Private Sub cmdOverwrite_Click()
    Dim lIndex As Long
    lIndex = lvwStoredTables.ListIndex
    mStorage.Remove lIndex + 1
    mStorage.Add mModel, lIndex
    lvwStoredTables.ListIndex = lIndex
End Sub

Private Sub cmdLoad_Click()
    Set mController.Model = mStorage.GetItems.Item(lvwStoredTables.ListIndex + 1)
End Sub

Private Sub cmdDelete_Click()
    mStorage.Remove lvwStoredTables.ListIndex + 1
End Sub


Private Sub cmdExportAll_Click()
    SaveAllStoredItems mStorage
End Sub

Private Sub CommandButton2_Click()
  frmAbout.Show
End Sub


Private Sub spnCellWidth_Change()
  txtCellSize.Text = spnCellWidth
End Sub

Private Sub spnIndent_Change()
  txtIndent.Text = spnIndent
End Sub

Private Sub txtCellSize_Change()
    On Error Resume Next
    spnCellWidth = txtCellSize
    If AutoApplyBox.Value Then mModel.CellWidth = txtCellSize
End Sub

Private Sub txtFilename_Change()
    mModel.FileName = txtFilename
    If txtFilename <> mModel.FileName Then
        txtFilename = mModel.FileName
    End If
End Sub

Private Sub txtIndent_Change()
    On Error Resume Next
    spnIndent = txtIndent
    If AutoApplyBox.Value Then mModel.Indent = txtIndent
End Sub

Private Sub cmdSelection_Click()
    Set mModel.Range = Application.Selection
    Me.cmdSelection.Caption = mModel.RangeAddress
End Sub

Private Sub UserForm_Click()
' This is regenerated every time the form is activated in the IDE. Just keep it here.
End Sub

Private Sub UserForm_Initialize()
    lvwStoredTables_Change
End Sub
