'Showcases use of Lambda functions and Generics.

#Import "<std>"
Using std..

Function Main()

	Local testStack := New Stack< MyObject >
	
	For Local n := 1 To 20
		Local newItem := New MyObject
		newItem.depth = Rnd( 0, 100 )
		testStack.Push( newItem )
	Next
		
	testStack.Sort( Lambda:Int( x:MyObject,y:MyObject )
		Return x.depth<=>y.depth
	End )
	
	For Local n := Eachin testStack
		Print( n.depth )
	Next
	
End


Struct MyObject
	Field depth := 0
End