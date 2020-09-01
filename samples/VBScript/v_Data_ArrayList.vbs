Class v_Data_ArrayList
	Private pArrayList

	Private Sub Class_Initialize()
		Set pArrayList = CreateObject("System.Collections.ArrayList")
	End Sub


	' Properties


	Public Property Get Capacity()
		Capacity = pArrayList.Capacity
	End Property

	Public Property Let Capacity(intSize)
		pArrayList.Capacity = intSize
	End Property

	Public Property Get Count()
		Count = pArrayList.Count
	End Property

	Public Property Get IsFixedSize()
		IsFixedSize = pArrayList.IsFixedSize
	End Property

	Public Property Get IsReadOnly()
		IsReadOnly = pArrayList.IsReadOnly
	End Property

	Public Property Get IsSynchronized()
		IsSynchronized = pArrayList.IsSynchronized
	End Property

	Public Default Property Get Item(intIndex)
		If IsObject(pArrayList(intIndex)) Then
			Set Item = pArrayList(intIndex)
		Else
			Item = pArrayList(intIndex)
		End If
	End Property

	Public Property Let Item(intIndex, varInput)
		pArrayList(intIndex) = varInput
	End Property

	Public Property Set Item(intIndex, varInput)
		Set pArrayList(intIndex) = varInput
	End Property

	Public Property Get SyncRoot()
		SyncRoot = pArrayList.SyncRoot
	End Property


	' Methods


	Public Sub Add(varItem)
		pArrayList.Add varItem
	End Sub

	Public Sub Clear()
		pArrayList.Clear()
	End Sub

	Public Function Clone()
		Set Clone = pArrayList.Clone()
	End Function

	Public Function Contains(varItem)
		Contains = pArrayList.Contains(varItem)
	End Function

	Public Function Equals(objItem)
		Equals = pArrayList.Equals(objItem)
	End Function

	Public Function GetEnumerator(intStart, intEnd)
		Set GetEnumerator = pArrayList.GetEnumerator(intStart, intEnd)
	End Function

	Public Function GetHashCode()
		GetHashCode = pArrayList.GetHashCode()
	End Function

	Public Function GetType()
		GetType = pArrayList.GetType()
	End Function

	Public Sub Insert(intIndex, varItem)
		pArrayList.Insert intIndex, varItem
	End Sub

	Public Sub Remove(varItem)
		pArrayList.Remove varItem
	End Sub

	Public Sub RemoveAt(intIndex)
		pArrayList.RemoveAt intIndex
	End Sub

	Public Sub Reverse()
		pArrayList.Reverse()
	End Sub

	Public Sub Sort()
		pArrayList.Sort()
	End Sub

	Public Function ToArray()
		ToArray = pArrayList.ToArray()
	End Function

	Public Function ToString()
		ToString = pArrayList.ToString()
	End Function

	Public Function TrimToSize()
		TrimToSize = pArrayList.TrimToSize()
	End Function

	Private Sub Class_Terminate()
		Set pArrayList = Nothing
	End Sub
End Class

If WScript.ScriptName = "v_Data_ArrayList.vbs" Then
	Dim arraylist
	Set arraylist = New v_Data_ArrayList

	arraylist.Add "Train"
	arraylist.Add "Bus"
	arraylist.Add "Car"
	arraylist.Add "Bicycle"
	arraylist.Add "Boat"

	WScript.Echo arraylist(2)
End If