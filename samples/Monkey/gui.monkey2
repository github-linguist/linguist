#Import "<mojo>"
#Import "<mojox>"

Using std..
Using mojo..
Using mojox..

Function Main()
	New AppInstance
	New TestGui
	App.Run()
End


Class TestGui Extends Window
	Field mainDock:DockingView
	Field rgtDock:ScrollView
	Field graphView:GraphView
	
	Const smallFont:Font = Font.Load( "font::DejaVuSans.ttf", 10 )
	
	Method New()					
		Super.New( "Test", 1024, 640, WindowFlags.Resizable )
		mainDock = New MainDock()
		rgtDock = New RightDock()
		mainDock.AddView( rgtDock, "right", "400", True  )
		ContentView = mainDock
	End
End


Class MainDock Extends DockingView
	Method New()
		Layout="fill"
		Local newStyle := Style.Copy()
		newStyle.BackgroundColor = Color.DarkGrey
		newStyle.BorderColor = Color.Black
		newStyle.Font = TestGui.smallFont
		Style = newStyle
	End
	
	Method OnRender( canvas:Canvas ) Override
		Super.OnRender( canvas )
		canvas.Color = New Color( Rnd(), Rnd(), Rnd() )
		canvas.DrawCircle( Frame.Width/4, Frame.Height/2, Frame.Height/4 )
		canvas.Color = Color.Aluminum
		canvas.DrawText( "gameview:" + App.FPS + " fps", 5, 5 )
	End
End



Class RightDock Extends ScrollView
	Private
	Field _panSpeed := 10.0
	
	Public
	Method New()
		Layout="fill"
		ScrollBarsVisible = True
		
		Local newStyle := Style.Copy()
		newStyle.BackgroundColor = Color.Grey
		newStyle.BorderColor = Color.Black
		newStyle.Font = TestGui.smallFont
		Style = newStyle
		
		Local graph:=New GraphView
		ContentView = graph
		
		Scroll = New Vec2i( graph.Frame.Width/2, graph.Frame.Height/2 )	'Doesn't work!
	End
		
	Method OnRender( canvas:Canvas ) Override
		Super.OnRender( canvas )
		canvas.Color = Color.Aluminum
		canvas.DrawText( "size:" + Frame + " ,scroll:" + Scroll , 5, 5 )
	End
	
	Method OnMouseEvent( event:MouseEvent ) Override
		Select event.Type
			Case EventType.MouseWheel
				Scroll = New Vec2i( Scroll.X+(event.Wheel.X*_panSpeed), Scroll.Y-(event.Wheel.Y*_panSpeed) )
				App.RequestRender()
		End
	End
End


Class GraphView	Extends View
	Private
	Field _panSpeed := 5.0
	Field _size := New Vec2i( 1024, 1024 )
	
	Public
	Method New()
		MinSize=New Vec2i( _size.X, _size.Y )
	End
	
	Method OnRender( canvas:Canvas ) Override
		Local r:= 20.0
		For Local x := 1 Until 10
			For Local y := 1 Until 10
				Local v :=  (x/10.0) -0.05
				canvas.Color = New Color( v, v, v )
				canvas.DrawCircle( (x*100)+r, (y*100)+r, r )
			Next
		Next
	End
End





