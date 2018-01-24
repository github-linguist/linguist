
Namespace example

#rem
multi
line
comment
#end

#rem
nested
#rem
multi
line
#end
comment
#end

'Importing a module pre-compile in the modules folder
#Import "<mojo>"

'Setting search paths for namespaces
Using mojo..
Using std..

Const ONECONST:Int = 1
Const TWOCONST := 2
Const THREECONST := 3, FOURCONST:Int = 4

Global someVariable:Int = 4

Function Main()
	'creating arrays
	Local scores:Int[]= New Int[](10,20,30)
	Local text:String[]= New String[]( "Hello","There","World" )
	
	' string type
	Local string1:String = "Hello world"
	Local string2:= "Hello world"
	
	' escape characers in strings
	Local string4 := "~qHello World~q"
	Local string5 := "~tIndented~n"
	Local string6 := "tilde is wavey... ~~"
	Print string4
	Print string5
	Print string6
	
	' string pseudofunctions
	Print "       Hello World  ~n".Trim()    ' prints "Hello World" whithout whitespace
	Print "Hello World".ToUpper()               ' prints "HELLO WORLD"

	' preprocessor keywords
	#If __TARGET__ = "android"
	'DoStuff()
	#ElseIf __TARGET__ = "ios"
	'DoOtherStuff()
	#End
	
	' operators
	Local a := 32
	Local b := 32 ~ 0
	b ~= 16
	b |= 16
	b &= 16
	Local c := a | b
	Print c
	
	'Creates a new Window class and starts the main App loop, using the Mojo module
	New AppInstance
	New GameWindow
	App.Run()
End


'------------------------------------------ Class Examples ------------------------------------------


'You can extend the Window class to customize its behavior
Class GameWindow Extends Window
	
	Private
	Field _spiral :Float[]
	Field _circle :Float[]
	
	Public
	Method New()
		Super.New( "Test", 800, 600, WindowFlags.Resizable )
	End
	
	'Properties can be used to create "read-only" values
	Property Spiral:Float[]()
		Return _spiral
	End
	
	'Or to control what happens to a value when assigned
	Property Circle:Float[]()
		Return _circle
	Setter( values:Float[] )
		If( values.Length > 2 ) And	( values.Length Mod 2 = 0 )
			_circle = values
		Else
			Print( "Circle values need to be an even number larger than 1" )	
		End
	End

	'Methods require a class instance. The keyword Self is optional when accessing fields and properties
	'The method Window.OnRender is virtual, and can be overriden
	'Width and Height are Propreties inherited from the Window class
	Method OnRender( canvas:Canvas ) Override
		RequestRender()
		canvas.Clear( Color.DarkGrey )
		canvas.Translate( Width/2.0, Height/2.0 )
		canvas.Rotate( -Millisecs()/200.0 )
		canvas.Color = New Color( 1, 0.8, 0.2 )
		DrawLines( canvas, Spiral )
		DrawLines( canvas, Circle, True )
	End
	
	'This method is called whenever the window layout changes, like when resizing
	Method OnLayout() Override
		_spiral = CreateSpiral( 0, 0, Height/1.5, Height/1.5, 100 )
		Circle = CreateCircle( 0, 0, Height/1.5, Height/1.5, 100 )	
	End
	
	'Functions can be called without a GameWindow instance, like "Static Functions" in other languages.
	Function DrawLines( canvas:Canvas, lines:Float[], closedShape:Bool = False )
		For Local n := 0 Until lines.Length Step 2
			Local l := lines.Length - 3
			Local x0 := lines[n]
			Local y0 := lines[n+1]
			Local x1 := n<l? lines[n+2] Else (closedShape? lines[0] Else x0 )    'Conditional assignment, uses the "?" symbol to test a condition
			Local y1 := n<l? lines[n+3] Else (closedShape? lines[1] Else y0 )
			canvas.DrawLine( x0, y0, x1, y1 )
		Next
	End
	
	Function CreateSpiral:Float[]( x:Double, y:Double, width:Double, height:Double, sides:Int = 32, turns:Float = 3.0 )
		Local stack := New Stack<Float>
		Local radStep := (Pi*2.0)/Float(sides)
		Local xMult := 0.0
		Local yMult := 0.0
		Local radiusX:Float = width/2.0
		Local radiusY:Float = height/2.0
		Local stepX:Float = radiusX / sides
		Local stepY:Float = radiusY / sides
		For Local a := 0.0 To Pi*2 Step radStep
			stack.Push( ( ( Sin( a*turns ) * radiusX )* xMult ) + x ) 
			stack.Push( ( ( Cos( a*turns ) * radiusY )* yMult ) + y )
			xMult += stepX/radiusX
			yMult += stepY/radiusY
		Next
		Return stack.ToArray()
	End
	
	Function CreateCircle:Float[]( x:Double, y:Double, width:Double, height:Double, sides:Int = 32 )
		Local stack := New Stack<Float>
		Local radStep := (Pi*2.0)/Float(sides)
		Local radiusX:Float = width/2.0
		Local radiusY:Float = height/2.0
		For Local a := 0.0 To Pi*2 Step radStep
			stack.Push( ( Sin( a ) * radiusX ) + x )
			stack.Push( ( Cos( a ) * radiusY ) + y )
		Next
		Return stack.ToArray()
	End

End

'--------- extending with generics -----------------------------------------------------------------------------

Class MyList Extends List<Double>
End

'--------- interfaces ------------------------------------------------------------------------------------------

Interface Computer
  Method Boot ()
  Method Process ()
  Method Display ()
End
'
Class PC Implements Computer
End

