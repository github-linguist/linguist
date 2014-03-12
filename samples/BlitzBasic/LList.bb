
; Double-linked list container class
;====================================

; with thanks to MusicianKool, for concept and issue fixes


Type LList
	Field head_.ListNode
	Field tail_.ListNode
End Type

Type ListNode
	Field pv_.ListNode
	Field nx_.ListNode
	Field Value
End Type

Type Iterator
	Field Value
	Field l_.LList
	Field cn_.ListNode, cni_
End Type


;Create a new LList object
Function CreateList.LList()
	Local l.LList = New LList
	
	l\head_ = New ListNode
	l\tail_ = New ListNode
	
	l\head_\nx_ = l\tail_		;End caps
	l\head_\pv_ = l\head_		;These make it more or less safe to iterate freely
	l\head_\Value = 0
	
	l\tail_\nx_ = l\tail_
	l\tail_\pv_ = l\head_
	l\tail_\Value = 0
	
	Return l
End Function

;Free a list and all elements (not any values)
Function FreeList(l.LList)
	ClearList l
	Delete l\head_
	Delete l\tail_
	Delete l
End Function

;Remove all the elements from a list (does not free values)
Function ClearList(l.LList)
	Local n.ListNode = l\head_\nx_
	While n <> l\tail_
		Local nx.ListNode = n\nx_
		Delete n
		n = nx
	Wend
	l\head_\nx_ = l\tail_
	l\tail_\pv_ = l\head_
End Function

;Count the number of elements in a list (slow)
Function ListLength(l.LList)
	Local i.Iterator = GetIterator(l), elems
	While EachIn(i)
		elems = elems + 1
	Wend
	Return elems
End Function

;Return True if a list contains a given value
Function ListContains(l.LList, Value)
	Return (ListFindNode(l, Value) <> Null)
End Function

;Create a linked list from the intvalues in a bank (slow)
Function ListFromBank.LList(bank)
	Local l.LList = CreateList()
	Local size = BankSize(bank), p
	
	For p = 0 To size - 4 Step 4
		ListAddLast l, PeekInt(bank, p)
	Next
	
	Return l
End Function

;Create a bank containing all the values in a list (slow)
Function ListToBank(l.LList)
	Local size = ListLength(l) * 4
	Local bank = CreateBank(size)
	
	Local i.Iterator = GetIterator(l), p = 0
	While EachIn(i)
		PokeInt bank, p, i\Value
		p = p + 4
	Wend
	
	Return bank
End Function

;Swap the contents of two list objects
Function SwapLists(l1.LList, l2.LList)
	Local tempH.ListNode = l1\head_, tempT.ListNode = l1\tail_
	l1\head_ = l2\head_
	l1\tail_ = l2\tail_
	l2\head_ = tempH
	l2\tail_ = tempT
End Function

;Create a new list containing the same values as the first
Function CopyList.LList(lo.LList)
	Local ln.LList = CreateList()
	Local i.Iterator = GetIterator(lo) : While EachIn(i)
		ListAddLast ln, i\Value
	Wend
	Return ln
End Function

;Reverse the order of elements of a list
Function ReverseList(l.LList)
	Local n1.ListNode, n2.ListNode, tmp.ListNode
	
	n1 = l\head_
	n2 = l\head_\nx_
	
	While n1 <> l\tail_
		n1\pv_ = n2
		tmp = n2\nx_
		n2\nx_ = n1
		n1 = n2
		n2 = tmp
	Wend
	
	tmp = l\head_
	l\head_ = l\tail_
	l\tail_ = tmp
	
	l\head_\pv_ = l\head_
	l\tail_\nx_ = l\tail_
End Function

;Search a list to retrieve the first node with the given value
Function ListFindNode.ListNode(l.LList, Value)
	Local n.ListNode = l\head_\nx_
	
	While n <> l\tail_
		If n\Value = Value Then Return n
		n = n\nx_
	Wend
	
	Return Null
End Function

;Append a value to the end of a list (fast) and return the node
Function ListAddLast.ListNode(l.LList, Value)
	Local n.ListNode = New ListNode
	
	n\pv_ = l\tail_\pv_
	n\nx_ = l\tail_
	n\Value = Value
	
	l\tail_\pv_ = n
	n\pv_\nx_ = n
	
	Return n
End Function

;Attach a value to the start of a list (fast) and return the node
Function ListAddFirst.ListNode(l.LList, Value)
	Local n.ListNode = New ListNode
	
	n\pv_ = l\head_
	n\nx_ = l\head_\nx_
	n\Value = Value
	
	l\head_\nx_ = n
	n\nx_\pv_ = n
	
	Return n
End Function

;Remove the first occurence of the given value from a list
Function ListRemove(l.LList, Value)
	Local n.ListNode = ListFindNode(l, Value)
	If n <> Null Then RemoveListNode n
End Function

;Remove a node from a list
Function RemoveListNode(n.ListNode)
	n\pv_\nx_ = n\nx_
	n\nx_\pv_ = n\pv_
	Delete n
End Function

;Return the value of the element at the given position from the start of the list,
;or backwards from the end of the list for a negative index
Function ValueAtIndex(l.LList, index)
	Local n.ListNode = ListNodeAtIndex(l, index)
	If n <> Null Then Return n\Value : Else Return 0
End Function

;Return the ListNode at the given position from the start of the list, or backwards
;from the end of the list for a negative index, or Null if invalid
Function ListNodeAtIndex.ListNode(l.LList, index)
	Local e, n.ListNode
	
	If index >= 0
		n = l\head_
		For e = 0 To index
			n = n\nx_
		Next
		If n = l\tail_ Then n = Null	;Beyond the end of the list - not valid
		
	Else	;Negative index - count backward
		n = l\tail_
		For e = 0 To index Step -1
			n = n\pv_
		Next
		If n = l\head_ Then n = Null	;Before the start of the list - not valid
		
	EndIf
	
	Return n
End Function

;Replace a value at the given position (added by MusicianKool)
Function ReplaceValueAtIndex(l.LList,index,value)
	Local n.ListNode = ListNodeAtIndex(l,index)
	If n <> Null Then n\Value = value:Else Return 0
End Function

;Remove and return a value at the given position (added by MusicianKool)
Function RemoveNodeAtIndex(l.LList,index)
	Local n.ListNode = ListNodeAtIndex(l,index),tval
	If n <> Null Then tval = n\Value:RemoveListNode(n):Return tval:Else Return 0
End Function

;Retrieve the first value from a list
Function ListFirst(l.LList)
	If l\head_\nx_ <> l\tail_ Then Return l\head_\nx_\Value
End Function

;Retrieve the last value from a list
Function ListLast(l.LList)
	If l\tail_\pv_ <> l\head_ Then Return l\tail_\pv_\Value
End Function

;Remove the first element from a list, and return its value
Function ListRemoveFirst(l.LList)
	Local val
	If l\head_\nx_ <> l\tail_
		val = l\head_\nx_\Value
		RemoveListNode l\head_\nx_
	EndIf
	Return val
End Function

;Remove the last element from a list, and return its value
Function ListRemoveLast(l.LList)
	Local val
	If l\tail_\pv_ <> l\head_
		val = l\tail_\pv_\Value
		RemoveListNode l\tail_\pv_
	EndIf
	Return val
End Function

;Insert a value into a list before the specified node, and return the new node
Function InsertBeforeNode.ListNode(Value, n.ListNode)
	Local bef.ListNode = New ListNode
	
	bef\pv_ = n\pv_
	bef\nx_ = n
	bef\Value = Value
	
	n\pv_ = bef
	bef\pv_\nx_ = bef
	
	Return bef
End Function

;Insert a value into a list after the specified node, and return then new node
Function InsertAfterNode.ListNode(Value, n.ListNode)
	Local aft.ListNode = New ListNode
	
	aft\nx_ = n\nx_
	aft\pv_ = n
	aft\Value = Value
	
	n\nx_ = aft
	aft\nx_\pv_ = aft
	
	Return aft
End Function

;Get an iterator object to use with a loop
;This function means that most programs won't have to think about deleting iterators manually
;(in general only a small, constant number will be created)
Function GetIterator.Iterator(l.LList)
	Local i.Iterator
	
	If l = Null Then RuntimeError "Cannot create Iterator for Null"
	
	For i = Each Iterator		;See if there's an available iterator at the moment
		If i\l_ = Null Then Exit
	Next
	
	If i = Null Then i = New Iterator	;If there wasn't, create one
	
	i\l_ = l
	i\cn_ = l\head_
	i\cni_ = -1
	i\Value = 0		;No especial reason why this has to be anything, but meh
	
	Return i
End Function

;Use as the argument to While to iterate over the members of a list
Function EachIn(i.Iterator)
	
	i\cn_ = i\cn_\nx_
	
	If i\cn_ <> i\l_\tail_		;Still items in the list
		i\Value = i\cn_\Value
		i\cni_ = i\cni_ + 1
		Return True
		
	Else
		i\l_ = Null		;Disconnect from the list, having reached the end
		i\cn_ = Null
		i\cni_ = -1
		Return False
		
	EndIf
End Function

;Remove from the containing list the element currently pointed to by an iterator
Function IteratorRemove(i.Iterator)
	If (i\cn_ <> i\l_\head_) And (i\cn_ <> i\l_\tail_)
		Local temp.ListNode = i\cn_
		
		i\cn_ = i\cn_\pv_
		i\cni_ = i\cni_ - 1
		i\Value = 0
		
		RemoveListNode temp
		
		Return True
	Else
		Return False
	EndIf
End Function

;Call this before breaking out of an EachIn loop, to disconnect the iterator from the list
Function IteratorBreak(i.Iterator)
	i\l_ = Null
	i\cn_ = Null
	i\cni_ = -1
	i\Value = 0
End Function


;~IDEal Editor Parameters:
;~F#5#A#10#18#2A#32#3E#47#4C#58#66#6F#78#8F#9B#A9#B7#BD#C5#CC
;~F#E3#E9#EF#F4#F9#103#10D#11B#12B#13F#152#163
;~C#Blitz3D