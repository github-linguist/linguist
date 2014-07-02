SuperStrict

Framework Brl.StandardIO

Type TMyType
	Field property:int

	Function A:int(param:int)
		'do nothing
	End Function

	Method B:int(param:int)
		'do nothing
	End Method
End Type


Global my:TMyType = new TMyType
?Win32
	my.A()
	my.B()
?Linux
	my.B()
	my.A()
?