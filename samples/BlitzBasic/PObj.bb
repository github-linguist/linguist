
Local i, start, result

Local s.Sum3Obj = New Sum3Obj

For i = 1 To 100000
	s = New Sum3Obj
	result = Handle Before s
	Delete s
Next

start = MilliSecs()
For i = 1 To 1000000
	result = Sum3_(MakeSum3Obj(i, i, i))
Next
start = MilliSecs() - start
Print start

start = MilliSecs()
For i = 1 To 1000000
	result = Sum3(i, i, i)
Next
start = MilliSecs() - start
Print start

WaitKey
End


Function Sum3(a, b, c)
	Return a + b + c
End Function


Type Sum3Obj
	Field isActive
	Field a, b, c
End Type

Function MakeSum3Obj(a, b, c)
	Local s.Sum3Obj = Last Sum3Obj
	If s\isActive Then s = New Sum3Obj
	s\isActive = True
	s\a = a
	s\b = b
	s\c = c
	
	Restore label
	Read foo
	
	Return Handle(s)
End Function

.label
Data (10 + 2), 12, 14
:
Function Sum3_(a_)
	Local a.Sum3Obj = Object.Sum3Obj a_
	Local return_ =  a\a + a\b + a\c
	Insert a Before First Sum3Obj :: a\isActive = False
	Return return_
End Function


;~IDEal Editor Parameters:
;~C#Blitz3D