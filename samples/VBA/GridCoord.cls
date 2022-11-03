VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
END
Attribute VB_Name = "GridCoord"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = False
Attribute VB_PredeclaredId = True
Attribute VB_Exposed = True
Attribute VB_Description = "An object representing a 2D grid coordinate."
'@Folder("Battleship.Model")
'@ModuleDescription("An object representing a 2D grid coordinate.")
'@PredeclaredId
'@Exposed
'@IgnoreModule UseMeaningfulName: X and Y are perfectly fine names in this context.
Option Explicit
Implements IGridCoord

Private Type TGridCoord
    X As Long
    Y As Long
End Type

Private this As TGridCoord

Public Function Create(ByVal xPosition As Long, ByVal yPosition As Long) As IGridCoord
    With New GridCoord
        .X = xPosition
        .Y = yPosition
        Set Create = .Self
    End With
End Function

Public Function FromString(ByVal coord As String) As IGridCoord
    coord = Replace(Replace(coord, "(", vbNullString), ")", vbNullString)
    
    Dim coords As Variant
    coords = Split(coord, ",")
    
    If UBound(coords) - LBound(coords) + 1 <> 2 Then Err.Raise 5, TypeName(Me), "Invalid format string"
    
    Dim xPosition As Long
    xPosition = coords(LBound(coords))
    
    Dim yPosition As Long
    yPosition = coords(UBound(coords))
    
    Set FromString = Create(xPosition, yPosition)
End Function

Public Property Get Self() As IGridCoord
    Set Self = Me
End Property

Public Property Get X() As Long
    X = this.X
End Property

Public Property Let X(ByVal value As Long)
    this.X = value
End Property

Public Property Get Y() As Long
    Y = this.Y
End Property

Public Property Let Y(ByVal value As Long)
    this.Y = value
End Property

Public Property Get Default() As IGridCoord
    Set Default = New GridCoord
End Property

Public Function ToString() As String
    ToString = "(" & this.X & "," & this.Y & ")"
End Function

Private Function IGridCoord_Equals(ByVal other As IGridCoord) As Boolean
    IGridCoord_Equals = other.X = this.X And other.Y = this.Y
End Function

Private Function IGridCoord_IsAdjacent(ByVal other As IGridCoord) As Boolean
    If other.Y = this.Y Then
        IGridCoord_IsAdjacent = other.X = this.X - 1 Or other.X = this.X + 1
    ElseIf other.X = this.X Then
        IGridCoord_IsAdjacent = other.Y = this.Y - 1 Or other.Y = this.Y + 1
    End If
End Function

Private Function IGridCoord_Offset(Optional ByVal xOffset As Long, Optional ByVal yOffset As Long) As IGridCoord
    Set IGridCoord_Offset = Create(this.X + xOffset, this.Y + yOffset)
End Function

Private Function IGridCoord_ToString() As String
    IGridCoord_ToString = Me.ToString
End Function

Private Function IGridCoord_ToA1String() As String
    IGridCoord_ToA1String = Chr$(64 + this.X) & this.Y
End Function

Private Property Get IGridCoord_X() As Long
    IGridCoord_X = this.X
End Property

Private Property Get IGridCoord_Y() As Long
    IGridCoord_Y = this.Y
End Property
