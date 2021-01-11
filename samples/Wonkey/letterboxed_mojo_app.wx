
Namespace myapp

#Import "<std>"
#Import "<mojo>"

Using std..
Using mojo..

Const Size:=New Vec2i( 640,360 )

Class MyWindow Extends Window

	Method New()
		Super.New( "My Window",640,480,WindowFlags.Resizable )
	
		Layout="letterbox"
	
	End

	Method OnRender( canvas:Canvas ) Override
	
		App.RequestRender()
	
		canvas.DrawText( "Hello World",Width/2,Height/2,.5,.5 )
	
	End
	
	Method OnKeyEvent( event:KeyEvent ) Override
	
		If event.Type=EventType.KeyDown And event.Key=Key.Enter And event.Modifiers & Modifier.Alt
		
			Fullscreen=Not Fullscreen
			
		Endif
	
	End
	
	Method OnMeasure:Vec2i() Override
	
		Return Size
		
	End
	
End

Function Main()

	New AppInstance
	
	New MyWindow
	
	App.Run()
End
