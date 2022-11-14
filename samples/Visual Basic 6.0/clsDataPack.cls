VERSION 1.0 CLASS
BEGIN
  MultiUse = -1  'True
  Persistable = 0  'NotPersistable
  DataBindingBehavior = 0  'vbNone
  DataSourceBehavior  = 0  'vbNone
  MTSTransactionMode  = 0  'NotAnMTSObject
END
Attribute VB_Name = "clsDataPack"
Attribute VB_GlobalNameSpace = False
Attribute VB_Creatable = True
Attribute VB_PredeclaredId = False
Attribute VB_Exposed = False
'[clsDataPack.cls]

'
' DataPack class by Alex Dragokas
'
' ver. 1.1
'
' Purpose: to (de)serialize series of variant data into byte array / hexed string
' (push / fetch order is the same, not a stack manner)
'

Option Explicit

Private Const INIT_RECORDS_COUNT As Long = 30

Private Type DATAPACK_RECORD
    Size    As Long
    Type    As VbVarType
    Data()  As Byte
End Type

Private g_Cell()    As DATAPACK_RECORD
Private g_Array()   As Byte
Private g_iPos      As Long
Private g_BytePos   As Long


Private Sub Class_Initialize()
    g_iPos = 0
    ReDim g_Cell(INIT_RECORDS_COUNT - 1)
End Sub

Private Sub Class_Terminate()
    Erase g_Array
    Erase g_Cell
End Sub

Public Sub Push(vData As Variant)
    On Error GoTo ErrorHandler:

    Dim ptr As Long

    g_iPos = g_iPos + 1
    If UBound(g_Cell) < g_iPos Then ReDim Preserve g_Cell(UBound(g_Cell) * 2 + 1)
    
    With g_Cell(g_iPos - 1)
    
        .Type = VarType(vData)
        ptr = VarDataPtr(vData)
    
        Select Case .Type
        Case vbString: .Size = Len(vData) * 2
        Case vbByte: .Size = 1
        Case vbInteger: .Size = 2
        Case vbBoolean: .Size = 2
        Case vbLong: .Size = 4
        Case vbCurrency: .Size = 8
        Case vbSingle: .Size = 4
        Case vbDouble: .Size = 8
        Case vbDate: .Size = 8
        Case Else
            g_iPos = g_iPos - 1
            Debug.Print "DataPack: Unknown type of data is pushed"
            Err.Raise 5
            Exit Sub
        End Select

        If .Size = 0 Then Exit Sub
        ReDim .Data(.Size - 1)
        memcpy .Data(0), ByVal ptr, .Size
    End With

    Exit Sub
ErrorHandler:
    ErrorMsg Err, "clsDataPack.Push", "Data: " & vData
    If inIDE Then Stop: Resume Next
End Sub

Public Function Fetch() As Variant
    On Error GoTo ErrorHandler:
    
    If 0 = AryItems(g_Array) Then
        Debug.Print "DataPack: Array is not specified before deserialization"
        Err.Raise 5
        Exit Function
    End If
    
    If g_BytePos > UBound(g_Array) Then
        Debug.Print "DataPack: EOF during deserialization"
        Err.Raise 5
        Exit Function
    End If
    
    Dim dr As DATAPACK_RECORD
    Dim v As Variant
    Dim ptr As Long
    
    memcpy dr, g_Array(g_BytePos), 6&                   'get header
    
    Select Case dr.Type
        Case vbString: v = String$(dr.Size \ 2, 0&)
        Case vbByte: v = CByte(0)
        Case vbInteger: v = CInt(0)
        Case vbBoolean: v = CBool(0)
        Case vbLong: v = CLng(0)
        Case vbCurrency: v = CCur(0)
        Case vbSingle: v = CSng(0)
        Case vbDouble: v = CDbl(0)
        Case vbDate: v = #12:00:00 AM#
        Case Else:
            Debug.Print "DataPack: Unknown type of data is fetched"
            Err.Raise 5
            Exit Function
    End Select
    
    If dr.Size > 0 Then
        ptr = VarDataPtr(v)
        memcpy ByVal ptr, g_Array(g_BytePos + 6), dr.Size
    End If
    g_BytePos = g_BytePos + dr.Size + 6
    
    Fetch = v
    
    Exit Function
ErrorHandler:
    ErrorMsg Err, "clsDataPack.Fetch", "Byte pos: " & g_BytePos
    If inIDE Then Stop: Resume Next
End Function

Private Function VarDataPtr(vData As Variant) As Long
    Const vbByRef As Integer = 16384
    Dim VT As Integer

    If VarType(vData) = vbString Then
        VarDataPtr = StrPtr(vData)
    Else
        GetMem2 vData, VT
        If VT And vbByRef Then
            GetMem4 ByVal VarPtr(vData) + 8, VarDataPtr 'VT_BYREF
        Else
            VarDataPtr = VarPtr(vData) + 8
        End If
    End If
End Function

Public Property Get SerializeToHexString() As String
    On Error GoTo ErrorHandler:
    Dim arr() As Byte
    Dim i As Long
    Dim s As String
    
    arr = SerializeToArray()
    
    If AryItems(arr) Then
        s = String$((UBound(arr) + 1) * 2, 0&)
        For i = 0 To UBound(arr)
            Mid$(s, i * 2 + 1) = Right$("0" & Hex$(arr(i)), 2)
        Next
    End If
    
    SerializeToHexString = s
    
    Exit Property
ErrorHandler:
    ErrorMsg Err, "clsDataPack.SerializeToHexString"
    If inIDE Then Stop: Resume Next
End Property

Public Property Get SerializeToArray() As Byte()
    On Error GoTo ErrorHandler:
    
    If g_iPos = 0 Then
        Debug.Print "DataPack: Cannot serialize - array is empty"
        Err.Raise 5
        Exit Property
    End If
    
    Dim i As Long
    Dim TSize As Long
    Dim iByte As Long
    
    For i = 0 To g_iPos - 1
        TSize = TSize + g_Cell(i).Size + 6 '6 is a sizeof ( Size and Type fields )
    Next
    
    ReDim g_Array(TSize - 1)
    iByte = 0
    
    'concat. datapack records
    For i = 0 To g_iPos - 1
        memcpy g_Array(iByte), ByVal VarPtr(g_Cell(i)), 6&               'header
        If g_Cell(i).Size > 0 Then
            memcpy g_Array(iByte + 6), g_Cell(i).Data(0), g_Cell(i).Size    'data
        End If
        iByte = iByte + g_Cell(i).Size + 6
    Next
    
    SerializeToArray = g_Array
    
    Exit Property
ErrorHandler:
    ErrorMsg Err, "clsDataPack.SerializeToArray"
    If inIDE Then Stop: Resume Next
End Property

Public Property Let DeSerializeArray(bData() As Byte)
    On Error GoTo ErrorHandler:

    If 0 = AryItems(bData) Then
        Debug.Print "DataPack: Cannot deserialize - array is empty"
        Err.Raise 5
        Exit Property
    End If
    
    g_Array = bData
    g_BytePos = 0
    
    Exit Property
ErrorHandler:
    ErrorMsg Err, "clsDataPack.DeSerializeArray"
    If inIDE Then Stop: Resume Next
End Property

Public Property Let DeSerializeHexString(sData As String)
    On Error GoTo ErrorHandler:

    Dim b() As Byte
    Dim i As Long
    
    If Len(sData) = 0 Then
        Debug.Print "DataPack: Cannot deserialize - string is empty"
        Err.Raise 5
        Exit Property
    End If
    
    ReDim b(Len(sData) \ 2 - 1)
    
    For i = 1 To Len(sData) Step 2
        b((i - 1) \ 2) = CLng("&H" & Mid$(sData, i, 2))
    Next
    
    DeSerializeArray = b
    
    Exit Property
ErrorHandler:
    ErrorMsg Err, "clsDataPack.DeSerializeHexString"
    If inIDE Then Stop: Resume Next
End Property

