' *************************************************************
'  
'  Licensed to the Apache Software Foundation (ASF) under one
'  or more contributor license agreements.  See the NOTICE file
'  distributed with this work for additional information
'  regarding copyright ownership.  The ASF licenses this file
'  to you under the Apache License, Version 2.0 (the
'  "License")' you may not use this file except in compliance
'  with the License.  You may obtain a copy of the License at
'  
'    http://www.apache.org/licenses/LICENSE-2.0
'  
'  Unless required by applicable law or agreed to in writing,
'  software distributed under the License is distributed on an
'  "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
'  KIND, either express or implied.  See the License for the
'  specific language governing permissions and limitations
'  under the License.
'  
' *************************************************************
VERSION 5.00
Begin {AC0714F6-3D04-11D1-AE7D-00A0C90F26F4} Wizard 
   ClientHeight    =   7470
   ClientLeft      =   1740
   ClientTop       =   1545
   ClientWidth     =   6585
   _ExtentX        =   11615
   _ExtentY        =   13176
   _Version        =   393216
   DisplayName     =   "AnalysisWizard"
   AppName         =   "Visual Basic"
   AppVer          =   "Visual Basic 6.0"
   LoadName        =   "Command Line / Startup"
   LoadBehavior    =   5
   RegLocation     =   "HKEY_CURRENT_USER\Software\Microsoft\Visual Basic\6.0"
   CmdLineSupport  =   -1  'True
End
Attribute VB_Name = "Wizard"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Option Explicit

Dim mcbMenuCommandBar         As Office.CommandBarControl  'command bar object
Public WithEvents MenuHandler As CommandBarEvents          'command bar event handler
Attribute MenuHandler.VB_VarHelpID = -1
Dim mfrmWizard As frmWizard
Dim VBInstance As VBIDE.VBE


'------------------------------------------------------
'this method adds the Add-In to the VB menu
'it is called by the VB addin manager
'------------------------------------------------------
Private Sub AddinInstance_OnConnection(ByVal application As Object, ByVal ConnectMode As AddInDesignerObjects.ext_ConnectMode, ByVal AddInInst As Object, custom() As Variant)
   On Error GoTo error_handler
   
   Set VBInstance = application
   
   If ConnectMode = ext_cm_External Then
       'Used by the wizard toolbar to start this wizard
       LoadMe
   Else
       Set mcbMenuCommandBar = AddToAddInCommandBar(VBInstance, LoadResString(15), LoadResPicture(5000, 0))
       'sink the event
       Set Me.MenuHandler = VBInstance.Events.CommandBarEvents(mcbMenuCommandBar)
   End If
  
   Exit Sub
     
error_handler:
   MsgBox Err.Description
End Sub

'------------------------------------------------------
'this method removes the Add-In from the VB menu
'it is called by the VB addin manager
'------------------------------------------------------
Private Sub AddinInstance_OnDisconnection(ByVal RemoveMode As AddInDesignerObjects.ext_DisconnectMode, custom() As Variant)
    'delete the command bar entry
    mcbMenuCommandBar.Delete
End Sub

'this event fires when the menu is clicked in the IDE
Private Sub MenuHandler_Click(ByVal CommandBarControl As Object, handled As Boolean, CancelDefault As Boolean)
    LoadMe
End Sub

Private Sub LoadMe()
    Set mfrmWizard = New frmWizard
    'pass the vb instance to the wizard module
    Set mfrmWizard.VBInst = VBInstance
    'load and show the form
    mfrmWizard.Show vbModal
    Set mfrmWizard = Nothing
End Sub


