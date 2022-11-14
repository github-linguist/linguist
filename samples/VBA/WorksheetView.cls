VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "WorksheetView"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
Attribute VB_Description = "Facade implementation for a Worksheet-based user interface."
'@Folder("Battleship.View.Worksheet")
'@ModuleDescription("Facade implementation for a Worksheet-based user interface.")
Option Explicit
Implements IGridViewCommands

Private adapter As GridViewAdapter ' IWeakReference
Private WithEvents sheetUI As GameSheet
Attribute sheetUI.VB_VarHelpID = -1

Private Sub Class_Initialize()
    Set sheetUI = GameSheet
End Sub

Private Sub Class_Terminate()
    Debug.Print TypeName(Me) & " is terminating"
End Sub

Private Property Get ViewEvents() As IGridViewEvents
    Set ViewEvents = adapter '.Object
End Property


':GameSheet event handlers
':Messages sent from the view
':***************************

Private Sub sheetUI_CreatePlayer(ByVal gridId As PlayGridId, ByVal pt As PlayerType, ByVal difficulty As AIDifficulty)
    ViewEvents.CreatePlayer gridId, pt, difficulty
End Sub

Private Sub sheetUI_DoubleClick(ByVal gridId As PlayGridId, ByVal position As IGridCoord, ByVal Mode As ViewMode)
    Select Case Mode
        
        Case ViewMode.FleetPosition
            ViewEvents.ConfirmShipPosition gridId, position
            
        Case ViewMode.Player1, ViewMode.Player2
            ViewEvents.AttackPosition gridId, position
            
    End Select
End Sub

Private Sub sheetUI_PlayerReady()
    ViewEvents.HumanPlayerReady
End Sub

Private Sub sheetUI_RightClick(ByVal gridId As PlayGridId, ByVal position As IGridCoord, ByVal Mode As ViewMode)
    If Mode = FleetPosition Then ViewEvents.PreviewRotateShip gridId, position
End Sub

Private Sub sheetUI_SelectionChange(ByVal gridId As PlayGridId, ByVal position As IGridCoord, ByVal Mode As ViewMode)
    If Mode = FleetPosition Then ViewEvents.PreviewShipPosition gridId, position
End Sub


':IGridViewCommands
':Messages sent from the controller
':*********************************

Private Property Set IGridViewCommands_Events(ByVal value As IGridViewEvents)
    Set adapter = value ' WeakReference.Create(Value)
End Property

Private Property Get IGridViewCommands_Events() As IGridViewEvents
    Set IGridViewCommands_Events = adapter '.Object
End Property

Private Sub IGridViewCommands_OnBeginAttack(ByVal currentPlayerGridId As PlayGridId)
    sheetUI.ShowInfoBeginAttackPhase currentPlayerGridId
End Sub

Private Sub IGridViewCommands_OnBeginShipPosition(ByVal CurrentShip As IShip, ByVal player As IPlayer)
    sheetUI.ShowInfoBeginDeployShip CurrentShip.Name
End Sub

Private Sub IGridViewCommands_OnBeginWaitForComputerPlayer()
    Application.Cursor = xlWait
    Application.StatusBar = "Please wait..."
End Sub

Private Sub IGridViewCommands_OnEndWaitForComputerPlayer()
    Application.Cursor = xlDefault
    Application.StatusBar = False
End Sub

Private Sub IGridViewCommands_OnConfirmShipPosition(ByVal player As IPlayer, ByVal newShip As IShip)
    sheetUI.ConfirmShipPosition player, newShip
End Sub
    
Private Sub IGridViewCommands_OnGameOver(ByVal winningGridId As PlayGridId)
    With sheetUI
        .ShowAnimationVictory winningGridId
        .ShowAnimationDefeat IIf(winningGridId = 1, 2, 1)
        .ShowReplayButton
        .LockGrids
    End With
    Application.Cursor = xlDefault
End Sub

Private Sub IGridViewCommands_OnHit(ByVal gridId As PlayGridId)
    With sheetUI
        .ShowAnimationHit gridId
        .LockGrid gridId
    End With
End Sub

Private Sub IGridViewCommands_OnInvalidShipPosition()
    sheetUI.ShowErrorInvalidShipPosition
End Sub

Private Sub IGridViewCommands_OnKnownPositionAttack()
    sheetUI.ShowErrorKnownPositionAttack
End Sub

Private Sub IGridViewCommands_OnLockGrid(ByVal gridId As PlayGridId)
    sheetUI.LockGrid gridId
End Sub

Private Sub IGridViewCommands_OnMiss(ByVal gridId As PlayGridId)
    With sheetUI
        .ShowAnimationMiss gridId
        .LockGrid gridId
    End With
End Sub

Private Sub IGridViewCommands_OnNewGame()
    With sheetUI
        .Visible = xlSheetVisible
        .OnNewGame
    End With
End Sub

Private Sub IGridViewCommands_OnPreviewShipPosition(ByVal player As IPlayer, ByVal newShip As IShip)
    sheetUI.PreviewShipPosition player, newShip
End Sub

Private Sub IGridViewCommands_OnRefreshGrid(ByVal grid As PlayerGrid)
    sheetUI.RefreshGrid grid
End Sub

Private Sub IGridViewCommands_OnSelectPosition(ByVal gridId As PlayGridId, ByVal position As IGridCoord)
    If sheetUI Is Application.ActiveSheet Then
        sheetUI.GridCoordToRange(gridId, position).Select
    End If
End Sub

Private Sub IGridViewCommands_OnSink(ByVal gridId As PlayGridId)
    With sheetUI
        .ShowAnimationSunk gridId
        .LockGrid gridId
    End With
End Sub

Private Sub IGridViewCommands_OnUpdateFleetStatus(ByVal player As IPlayer, ByVal hitShip As IShip, Optional ByVal showAIStatus As Boolean = False)
    With sheetUI
        If player.PlayerType = ComputerControlled And showAIStatus Then
            .ShowAcquiredTarget IIf(player.PlayGrid.gridId = 1, 1, 2), hitShip.Name, hitShip.IsSunken
        Else
            .UpdateShipStatus player, hitShip
        End If
    End With
End Sub

