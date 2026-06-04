Attribute VB_Name = "QuickCards"
Option Explicit

Public Sub AddQuickCard()
    Dim Profile As String
    Dim t As Template
    Dim Name As String
    Dim i As Long
    Dim j As Long
    Dim k As Long
    
    On Error GoTo Handler
    
    If Selection.Start = Selection.End Then
        MsgBox "You must select some text to save a Quick Card", vbOKOnly
        Exit Sub
    End If
    
    Name = InputBox("What shortcut word/phrase do you want to use for your Quick Card? Usually this is the author's last name.", "Add Quick Card", "")
    If Name = "" Then Exit Sub
    
    Profile = GetSetting("Verbatim", "QuickCards", "QuickCardsProfile", "Verbatim1")
    If Not Profile Like "Verbatim*" Then Profile = "Verbatim1"

    Set t = ActiveDocument.AttachedTemplate
          
    For i = 1 To t.BuildingBlockTypes.Count
        If t.BuildingBlockTypes.Item(i).Name = "Custom 1" Then
            For j = 1 To t.BuildingBlockTypes.Item(i).Categories.Count
                If t.BuildingBlockTypes.Item(i).Categories.Item(j).Name = Profile Then
                    For k = 1 To t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Count
                        If LCase$(t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Item(k).Name) = LCase$(Name) Then
                            MsgBox "There's already a Quick Card with that name, try again with a different name!", vbOKOnly, "Failed To Add Quick Card"
                            Exit Sub
                        End If
                    Next k
                End If
            Next j
        End If
    Next i
    
    t.BuildingBlockEntries.Add Name, wdTypeCustom1, Profile, Selection.Range
    
    t.Save

    Ribbon.RefreshRibbon
    
    MsgBox "Successfully created Quick Card with the shortcut """ & Name & """"

    Set t = Nothing
    Exit Sub

Handler:
    Set t = Nothing
    MsgBox "Error " & Err.Number & ": " & Err.Description
End Sub

'@Ignore ProcedureNotUsed
Public Sub InsertCurrentQuickCard()
    On Error Resume Next
    Selection.Range.InsertAutoText
    On Error GoTo 0
End Sub

Public Sub InsertQuickCard(ByRef QuickCardName As String)
    Dim Profile As String
    Dim t As Template
    Dim i As Long
    Dim j As Long
    Dim k As Long
    
    On Error GoTo Handler
    
    Profile = GetSetting("Verbatim", "QuickCards", "QuickCardsProfile", "Verbatim1")
    If Not Profile Like "Verbatim*" Then Profile = "Verbatim1"
    
    Set t = ActiveDocument.AttachedTemplate
    
    For i = 1 To t.BuildingBlockTypes.Count
        If t.BuildingBlockTypes.Item(i).Name = "Custom 1" Then
            For j = 1 To t.BuildingBlockTypes.Item(i).Categories.Count
                If t.BuildingBlockTypes.Item(i).Categories.Item(j).Name = Profile Then
                    For k = 1 To t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Count
                        If LCase$(t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Item(k).Name) = LCase$(QuickCardName) Then
                            t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Item(k).Insert Selection.Range, True
                        End If
                    Next k
                End If
            Next j
        End If
    Next i
    
    Set t = Nothing
    Exit Sub

Handler:
    Set t = Nothing
    MsgBox "Error " & Err.Number & ": " & Err.Description
End Sub

Public Sub DeleteQuickCard(Optional ByRef QuickCardName As String)
    Dim Profile As String
    Dim t As Template
    Dim i As Long
    Dim j As Long
    Dim k As Long
    
    On Error GoTo Handler
    
    If QuickCardName <> "" Or IsNull(QuickCardName) Then
        If MsgBox("Are you sure you want to delete the Quick Card """ & QuickCardName & """? This cannot be reversed.", vbYesNo, "Are you sure?") = vbNo Then Exit Sub
    Else
        If MsgBox("Are you sure you want to delete all saved Quick Cards? This cannot be reversed.", vbYesNo, "Are you sure?") = vbNo Then Exit Sub
    End If
    
    Profile = GetSetting("Verbatim", "QuickCards", "QuickCardsProfile", "Verbatim1")
    If Not Profile Like "Verbatim*" Then Profile = "Verbatim1"
    
    Set t = ActiveDocument.AttachedTemplate

    ' Delete all building blocks in the Custom 1/Verbatim category
    For i = 1 To t.BuildingBlockTypes.Count
        If t.BuildingBlockTypes.Item(i).Name = "Custom 1" Then
            For j = 1 To t.BuildingBlockTypes.Item(i).Categories.Count
                If t.BuildingBlockTypes.Item(i).Categories.Item(j).Name = Profile Then
                    For k = t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Count To 1 Step -1
                        ' If name provided, delete just that building block, otherwise delete everything in the category
                        If QuickCardName <> "" Or IsNull(QuickCardName) Then
                            If t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Item(k).Name = QuickCardName Then
                                t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Item(k).Delete
                            End If
                        Else
                            t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Item(k).Delete
                        End If
                    Next k
                End If
            Next j
        End If
    Next i

    t.Save
    Set t = Nothing
        
    Exit Sub

Handler:
    Set t = Nothing
    MsgBox "Error " & Err.Number & ": " & Err.Description
End Sub

'@Ignore ParameterNotUsed, ProcedureNotUsed
'@Ignore ProcedureCanBeWrittenAsFunction
Public Sub GetQuickCardsContent(ByVal c As IRibbonControl, ByRef returnedVal As Variant)
' Get content for dynamic menu for Quick Cards
    Dim Profile As String
    Dim t As Template
    Dim i As Long
    Dim j As Long
    Dim k As Long
    Dim xml As String
    Dim QuickCardName As String
    Dim DisplayName As String
       
    On Error Resume Next
        
    Profile = GetSetting("Verbatim", "QuickCards", "QuickCardsProfile", "Verbatim1")
    If Not Profile Like "Verbatim*" Then Profile = "Verbatim1"
        
    Set t = ActiveDocument.AttachedTemplate

    ' Start the menu
    xml = "<menu xmlns=""http://schemas.microsoft.com/office/2006/01/customui"">"
    
    ' Populate the list of current Quick Cards in the Custom 1 / Verbatim gallery
    For i = 1 To t.BuildingBlockTypes.Count
        If t.BuildingBlockTypes.Item(i).Name = "Custom 1" Then
            For j = 1 To t.BuildingBlockTypes.Item(i).Categories.Count
                If t.BuildingBlockTypes.Item(i).Categories.Item(j).Name = Profile Then
                    For k = 1 To t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Count
                         QuickCardName = t.BuildingBlockTypes.Item(i).Categories.Item(j).BuildingBlocks.Item(k).Name
                         DisplayName = Strings.OnlySafeChars(QuickCardName)
                        xml = xml & "<button id=""QuickCard" & Replace(DisplayName, " ", "") & """ label=""" & DisplayName & """ tag=""" & QuickCardName & """ onAction=""QuickCards.InsertQuickCardFromRibbon"" imageMso=""AutoSummaryResummarize"" />"
                    Next k
                End If
            Next j
        End If
    Next i
    
    ' Close the menu
    xml = xml & "<button id=""QuickCardSettings"" label=""Quick Card Settings"" onAction=""Ribbon.RibbonMain"" imageMso=""AddInManager""" & " />"
    xml = xml & "</menu>"
    
    Set t = Nothing
    
    returnedVal = xml
        
    On Error GoTo 0
        
    Exit Sub
End Sub

'@Ignore ProcedureNotUsed
Public Sub InsertQuickCardFromRibbon(ByVal c As IRibbonControl)
    QuickCards.InsertQuickCard c.Tag
End Sub
