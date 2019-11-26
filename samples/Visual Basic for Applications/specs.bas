Attribute VB_Name = "Specs"
Private pForDisplay As Boolean
Private pUseNative As Boolean

Public Sub SpeedTest()
    #If Mac Then
        ' Mac
        ExecuteSpeedTest CompareToNative:=False
    #Else
        ' Windows
        ExecuteSpeedTest CompareToNative:=True
    #End If
End Sub

Sub ToggleNative(Optional Enabled As Boolean = True)
    Dim Code As CodeModule
    Dim Lines As Variant
    Dim i As Integer
    
    Set Code = ThisWorkbook.VBProject.VBComponents("Dictionary").CodeModule
    Lines = Split(Code.Lines(1, 50), vbNewLine)
    
    For i = 0 To UBound(Lines)
        If InStr(1, Lines(i), "#Const UseScriptingDictionaryIfAvailable") Then
            Code.ReplaceLine i + 1, "#Const UseScriptingDictionaryIfAvailable = " & Enabled
            Exit Sub
        End If
    Next i
End Sub

Public Sub RunSpecs()
    DisplayRunner.IdCol = 1
    DisplayRunner.DescCol = 1
    DisplayRunner.ResultCol = 2
    DisplayRunner.OutputStartRow = 4
    
    pForDisplay = True
    DisplayRunner.RunSuite Specs()
    pForDisplay = False
End Sub

Public Function Specs() As SpecSuite
    Dim UseNative As Boolean

#If Mac Then
    UseNative = False
#Else
    If pUseNative Then
        UseNative = True
        pUseNative = False
    Else
        If Not pForDisplay Then
            ' Run native specs first
            pUseNative = True
            Specs
        End If
        
        UseNative = False
    End If
#End If

    Set Specs = New SpecSuite
    If UseNative Then
        Specs.Description = "Scripting.Dictionary"
    Else
        Specs.Description = "VBA-Dictionary"
    End If
    
    Dim Dict As Object
    Dim Items As Variant
    Dim Keys As Variant
    Dim Key As Variant
    Dim Item As Variant
    Dim A As New Collection
    Dim B As New Dictionary
    
    ' Properties
    ' ------------------------- '
    With Specs.It("should get count of items")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        .Expect(Dict.Count).ToEqual 3
        
        Dict.Remove "C"
        .Expect(Dict.Count).ToEqual 2
    End With
    
    With Specs.It("should get item by key")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        
        .Expect(Dict.Item("B")).ToEqual 3.14
        .Expect(Dict.Item("D")).ToBeEmpty
        .Expect(Dict("B")).ToEqual 3.14
        .Expect(Dict("D")).ToBeEmpty
    End With
    
    With Specs.It("should let item by key")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        
        ' Let + New
        Dict("D") = True
        
        ' Let + Replace
        Dict("A") = 456
        Dict("B") = 3.14159
        
        ' Should have correct values
        .Expect(Dict("A")).ToEqual 456
        .Expect(Dict("B")).ToEqual 3.14159
        .Expect(Dict("C")).ToEqual "ABC"
        .Expect(Dict("D")).ToEqual True
        
        ' Should have correct order
        .Expect(Dict.Keys()(0)).ToEqual "A"
        .Expect(Dict.Keys()(1)).ToEqual "B"
        .Expect(Dict.Keys()(2)).ToEqual "C"
        .Expect(Dict.Keys()(3)).ToEqual "D"
    End With
    
    With Specs.It("should set item by key")
        Set Dict = CreateDictionary(UseNative)

        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"

        ' Set + New
        Set Dict("D") = CreateDictionary(UseNative)
        Dict("D").Add "key", "D"

        ' Set + Replace
        Set Dict("A") = CreateDictionary(UseNative)
        Dict("A").Add "key", "A"
        Set Dict("B") = CreateDictionary(UseNative)
        Dict("B").Add "key", "B"

        ' Should have correct values
        .Expect(Dict.Item("A")("key")).ToEqual "A"
        .Expect(Dict.Item("B")("key")).ToEqual "B"
        .Expect(Dict.Item("C")).ToEqual "ABC"
        .Expect(Dict.Item("D")("key")).ToEqual "D"

        ' Should have correct order
        .Expect(Dict.Keys()(0)).ToEqual "A"
        .Expect(Dict.Keys()(1)).ToEqual "B"
        .Expect(Dict.Keys()(2)).ToEqual "C"
        .Expect(Dict.Keys()(3)).ToEqual "D"
    End With
    
    With Specs.It("should change key")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        
        Dict.Key("B") = "PI"
        .Expect(Dict("PI")).ToEqual 3.14
    End With
    
    With Specs.It("should use CompareMode")
        Set Dict = CreateDictionary(UseNative)
        Dict.CompareMode = 0
        
        Dict.Add "A", 123
        Dict("a") = 456
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        
        .Expect(Dict("A")).ToEqual 123
        .Expect(Dict("a")).ToEqual 456
        
        Set Dict = CreateDictionary(UseNative)
        Dict.CompareMode = 1
        
        Dict.Add "A", 123
        Dict("a") = 456
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        
        .Expect(Dict("A")).ToEqual 456
        .Expect(Dict("a")).ToEqual 456
    End With
    
    With Specs.It("should allow Variant for key")
        Set Dict = CreateDictionary(UseNative)
        
        Key = "A"
        Dict(Key) = 123
        .Expect(Dict(Key)).ToEqual 123
        
        Key = "B"
        Set Dict(Key) = CreateDictionary(UseNative)
        .Expect(Dict(Key).Count).ToEqual 0
    End With
    
    With Specs.It("should handle numeric keys")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add 3, 1
        Dict.Add 2, 2
        Dict.Add 1, 3
        Dict.Add "3", 4
        Dict.Add "2", 5
        Dict.Add "1", 6

        .Expect(Dict(3)).ToEqual 1
        .Expect(Dict(2)).ToEqual 2
        .Expect(Dict(1)).ToEqual 3
        .Expect(Dict("3")).ToEqual 4
        .Expect(Dict("2")).ToEqual 5
        .Expect(Dict("1")).ToEqual 6
        
        .Expect(Dict.Keys()(0)).ToEqual 3
        .Expect(Dict.Keys()(1)).ToEqual 2
        .Expect(Dict.Keys()(2)).ToEqual 1
        .Expect(TypeName(Dict.Keys()(0))).ToEqual "Integer"
        .Expect(TypeName(Dict.Keys()(1))).ToEqual "Integer"
        .Expect(TypeName(Dict.Keys()(2))).ToEqual "Integer"
    End With
    
    With Specs.It("should handle boolean keys")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add True, 1
        Dict.Add False, 2
        
        .Expect(Dict(True)).ToEqual 1
        .Expect(Dict(False)).ToEqual 2
        
        .Expect(Dict.Keys()(0)).ToEqual True
        .Expect(Dict.Keys()(1)).ToEqual False
        .Expect(TypeName(Dict.Keys()(0))).ToEqual "Boolean"
        .Expect(TypeName(Dict.Keys()(1))).ToEqual "Boolean"
    End With
    
    With Specs.It("should handle object keys")
        Set Dict = CreateDictionary(UseNative)
        
        Set A = New Collection
        Set B = New Dictionary
        
        A.Add 123
        B.Add "a", 456
        
        Dict.Add A, "123"
        Dict.Add B, "456"
        
        .Expect(Dict(A)).ToEqual "123"
        .Expect(Dict(B)).ToEqual "456"
        
        Dict.Remove B
        Dict.Key(A) = B
        
        .Expect(Dict(B)).ToEqual "123"
    End With
    
    ' Methods
    ' ------------------------- '
    With Specs.It("should add an item")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        Dict.Add "D", True
        Dict.Add "E", Array(1, 2, 3)
        Dict.Add "F", Dict
        
        .Expect(Dict("A")).ToEqual 123
        .Expect(Dict("B")).ToEqual 3.14
        .Expect(Dict("C")).ToEqual "ABC"
        .Expect(Dict("D")).ToEqual True
        .Expect(Dict("E")(1)).ToEqual 2
        .Expect(Dict("F")("C")).ToEqual "ABC"
    End With
    
    With Specs.It("should check if an item exists")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add "Exists", 123
        .Expect(Dict.Exists("Exists")).ToEqual True
        .Expect(Dict.Exists("Doesn't Exist")).ToEqual False
    End With
    
    With Specs.It("should get an array of all items")
        Set Dict = CreateDictionary(UseNative)
        
        .Expect(Dict.Items).RunMatcher "Specs.ToBeAnEmptyArray", "to be an empty array"
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        Dict.Add "D", True
        
        Items = Dict.Items
        .Expect(UBound(Items)).ToEqual 3
        .Expect(Items(0)).ToEqual 123
        .Expect(Items(3)).ToEqual True
        
        Dict.Remove "A"
        Dict.Remove "B"
        Dict.Remove "C"
        Dict.Remove "D"
        .Expect(Dict.Items).RunMatcher "Specs.ToBeAnEmptyArray", "to be an empty array"
    End With
    
    With Specs.It("should get an array of all keys")
        Set Dict = CreateDictionary(UseNative)
        
        .Expect(Dict.Keys).RunMatcher "Specs.ToBeAnEmptyArray", "to be an empty array"
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        Dict.Add "D", True
        
        Keys = Dict.Keys
        .Expect(UBound(Keys)).ToEqual 3
        .Expect(Keys(0)).ToEqual "A"
        .Expect(Keys(3)).ToEqual "D"
        
        Dict.RemoveAll
        .Expect(Dict.Keys).RunMatcher "Specs.ToBeAnEmptyArray", "to be an empty array"
    End With
    
    With Specs.It("should remove item")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        Dict.Add "D", True
        
        .Expect(Dict.Count).ToEqual 4
        
        Dict.Remove "C"
                
        .Expect(Dict.Count).ToEqual 3
    End With
    
    With Specs.It("should remove all items")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        Dict.Add "D", True
        
        .Expect(Dict.Count).ToEqual 4
        
        Dict.RemoveAll
        
        .Expect(Dict.Count).ToEqual 0
    End With
    
    ' Other
    ' ------------------------- '
    With Specs.It("should For Each over keys")
        Set Dict = CreateDictionary(UseNative)
        
        Set Keys = New Collection
        For Each Key In Dict.Keys
            Keys.Add Key
        Next Key
        
        .Expect(Keys.Count).ToEqual 0
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        Dict.Add "D", True
        
        Set Keys = New Collection
        For Each Key In Dict.Keys
            Keys.Add Key
        Next Key
        
        .Expect(Keys.Count).ToEqual 4
        .Expect(Keys(1)).ToEqual "A"
        .Expect(Keys(4)).ToEqual "D"
    End With
    
    With Specs.It("should For Each over items")
        Set Dict = CreateDictionary(UseNative)
        
        Set Items = New Collection
        For Each Item In Dict.Items
            Items.Add Item
        Next Item
        
        .Expect(Items.Count).ToEqual 0
        
        Dict.Add "A", 123
        Dict.Add "B", 3.14
        Dict.Add "C", "ABC"
        Dict.Add "D", True
        
        Set Items = New Collection
        For Each Item In Dict.Items
            Items.Add Item
        Next Item
        
        .Expect(Items.Count).ToEqual 4
        .Expect(Items(1)).ToEqual 123
        .Expect(Items(4)).ToEqual True
    End With
    
    With Specs.It("should have UBound of -1 for empty Keys and Items")
        Set Dict = CreateDictionary(UseNative)
        
        .Expect(UBound(Dict.Keys)).ToEqual -1
        .Expect(UBound(Dict.Items)).ToEqual -1
        .Expect(Err.Number).ToEqual 0
    End With
    
    ' Errors
    ' ------------------------- '
    Err.Clear
    On Error Resume Next
    
    With Specs.It("should throw 5 when changing CompareMode with items in Dictionary")
        Set Dict = CreateDictionary(UseNative)
        Dict.Add "A", 123
        
        Dict.CompareMode = vbTextCompare
        
        .Expect(Err.Number).ToEqual 5
        Err.Clear
    End With
    
    With Specs.It("should throw 457 on Add if key exists")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add "A", 123
        Dict.Add "A", 456
        
        .Expect(Err.Number).ToEqual 457
        Err.Clear
        
        Dict.RemoveAll
        Dict.Add "A", 123
        Dict.Add "a", 456
        
        .Expect(Err.Number).ToEqual 0
        Err.Clear
        
        Dict.RemoveAll
        Dict.CompareMode = vbTextCompare
        Dict.Add "A", 123
        Dict.Add "a", 456
        
        .Expect(Err.Number).ToEqual 457
        Err.Clear
    End With
    
    With Specs.It("should throw 32811 on Remove if key doesn't exist")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Remove "A"
        
        .Expect(Err.Number).ToEqual 32811
        Err.Clear
    End With
    
    With Specs.It("should throw 457 for Boolean key quirks")
        Set Dict = CreateDictionary(UseNative)
        
        Dict.Add True, "abc"
        Dict.Add -1, "def"
        
        .Expect(Err.Number).ToEqual 457
        Err.Clear
        
        Dict.Add False, "abc"
        Dict.Add 0, "def"
        
        .Expect(Err.Number).ToEqual 457
        Err.Clear
    End With
    
    On Error GoTo 0
    InlineRunner.RunSuite Specs
End Function

Public Sub ExecuteSpeedTest(Optional CompareToNative As Boolean = False)
    Dim Counts As Variant
    Counts = Array(5000, 5000, 5000, 5000, 7500, 7500, 7500, 7500)
    
    Dim Baseline As Collection
    If CompareToNative Then
        Set Baseline = RunSpeedTest(Counts, True)
    End If
    
    Dim Results As Collection
    Set Results = RunSpeedTest(Counts, False)
    
    Debug.Print vbNewLine & "SpeedTest Results:" & vbNewLine
    PrintResults "Add", Baseline, Results, 0
    PrintResults "Iterate", Baseline, Results, 1
End Sub

Public Sub PrintResults(Test As String, Baseline As Collection, Results As Collection, Index As Integer)
    Dim BaselineAvg As Single
    Dim ResultsAvg As Single
    Dim i As Integer
    
    If Not Baseline Is Nothing Then
        For i = 1 To Baseline.Count
            BaselineAvg = BaselineAvg + Baseline(i)(Index)
        Next i
        BaselineAvg = BaselineAvg / Baseline.Count
    End If
    
    For i = 1 To Results.Count
        ResultsAvg = ResultsAvg + Results(i)(Index)
    Next i
    ResultsAvg = ResultsAvg / Results.Count
    
    Dim Result As String
    Result = Test & ": " & Format(Round(ResultsAvg, 0), "#,##0") & " ops./s"
    
    If Not Baseline Is Nothing Then
        Result = Result & " vs. " & Format(Round(BaselineAvg, 0), "#,##0") & " ops./s "
    
        If ResultsAvg < BaselineAvg Then
            Result = Result & Format(Round(BaselineAvg / ResultsAvg, 0), "#,##0") & "x slower"
        ElseIf BaselineAvg > ResultsAvg Then
            Result = Result & Format(Round(ResultsAvg / BaselineAvg, 0), "#,##0") & "x faster"
        End If
    End If
    Result = Result
    
    If Results.Count > 1 Then
        Result = Result & vbNewLine
        For i = 1 To Results.Count
            Result = Result & "  " & Format(Round(Results(i)(Index), 0), "#,##0")
            
            If Not Baseline Is Nothing Then
                Result = Result & " vs. " & Format(Round(Baseline(i)(Index), 0), "#,##0")
            End If
            
            Result = Result & vbNewLine
        Next i
    End If
    
    Debug.Print Result
End Sub

Public Function RunSpeedTest(Counts As Variant, Optional UseNative As Boolean = False) As Collection
    Dim Results As New Collection
    Dim CountIndex As Integer
    Dim Dict As Object
    Dim i As Long
    Dim AddResult As Double
    Dim Key As Variant
    Dim Value As Variant
    Dim IterateResult As Double
    Dim Timer As New PreciseTimer
    
    For CountIndex = LBound(Counts) To UBound(Counts)
        Timer.StartTimer
    
        Set Dict = CreateDictionary(UseNative)
        For i = 1 To Counts(CountIndex)
            Dict.Add CStr(i), i
        Next i
        
        ' Convert to seconds
        AddResult = Timer.TimeElapsed / 1000#
        
        ' Convert to ops./s
        If AddResult > 0 Then
            AddResult = Counts(CountIndex) / AddResult
        Else
            ' Due to single precision, timer resolution is 0.01 ms, set to 0.005 ms
            AddResult = Counts(CountIndex) / 0.005
        End If
        
        Timer.StartTimer
        
        For Each Key In Dict.Keys
            Value = Dict.Item(Key)
        Next Key
        
        ' Convert to seconds
        IterateResult = Timer.TimeElapsed / 1000#
        
        ' Convert to ops./s
        If IterateResult > 0 Then
            IterateResult = Counts(CountIndex) / IterateResult
        Else
            ' Due to single precision, timer resolution is 0.01 ms, set to 0.005 ms
            IterateResult = Counts(CountIndex) / 0.005
        End If
        
        Results.Add Array(AddResult, IterateResult)
    Next CountIndex
    
    Set RunSpeedTest = Results
End Function

Public Function CreateDictionary(Optional UseNative As Boolean = False) As Object
    If UseNative Then
        Set CreateDictionary = CreateObject("Scripting.Dictionary")
    Else
        Set CreateDictionary = New Dictionary
    End If
End Function

Public Function ToBeAnEmptyArray(Actual As Variant) As Variant
    Dim UpperBound As Long

    Err.Clear
    On Error Resume Next
    
    ' First, make sure it's an array
    If IsArray(Actual) = False Then
        ' we weren't passed an array, return True
        ToBeAnEmptyArray = True
    Else
        ' Attempt to get the UBound of the array. If the array is
        ' unallocated, an error will occur.
        UpperBound = UBound(Actual, 1)
        If (Err.Number <> 0) Then
            ToBeAnEmptyArray = True
        Else
            ' Check for case of -1 UpperBound (Scripting.Dictionary.Keys/Items)
            Err.Clear
            If LBound(Actual) > UpperBound Then
                ToBeAnEmptyArray = True
            Else
                ToBeAnEmptyArray = False
            End If
        End If
    End If
End Function
