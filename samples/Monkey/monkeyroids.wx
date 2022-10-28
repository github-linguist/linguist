
Namespace asteroids

#Import "<std>"
#Import "<mojo>"

Using std..
Using mojo..

Const SCREEN_WIDTH:Int = 640
Const SCREEN_HEIGHT:Int = 480
Const PI:Float = 3.14159265358979323846

Class MyWindow Extends Window
	Const STATE_TITLE:Int= 1
	Const STATE_GAME:Int = 2
	Const STATE_GAME_OVER:Int = 3
	Global GameState:Int = STATE_TITLE
	
	Field player:Player
	Field score:Int
	Field level:Int
	Field bestScore:Int
	Field asteroidsNum:Int
	Field asteroidsSize:Int
		
	Method New(title:String, width:Int, height:Int)
		Super.New(title, width, height)
		ClearColor = Color.Black
		SwapInterval = 1
		SeedRnd(Millisecs())
		Reset()
	End
	
	Method OnWindowEvent(event:WindowEvent) Override
		Select event.Type
			Case EventType.WindowMoved
			Case EventType.WindowResized
				App.RequestRender()
			Case EventType.WindowGainedFocus
			Case EventType.WindowLostFocus
			Default
				Super.OnWindowEvent(event)
		End
	End
	
	Method OnRender(canvas:Canvas) Override
		App.RequestRender()
		
		Select GameState
			Case STATE_TITLE
				Asteroid.UpdateAll()
				If Keyboard.KeyHit(Key.Space)
			   		SetState(STATE_GAME)
				End	
			Case STATE_GAME
				player.Update()
				Bullet.UpdateAll()
				Asteroid.UpdateAll()
				CheckCollisions()
			Case STATE_GAME_OVER
				Asteroid.UpdateAll()
				If Keyboard.KeyHit(Key.Space)
			   		SetState(STATE_TITLE)
					Reset()
				End
		End
		
		Select GameState
			Case STATE_TITLE
				DrawTitleScreen(canvas)
			Case STATE_GAME
				DrawGame(canvas)
			Case STATE_GAME_OVER
				DrawGameOver(canvas)
		End
	End
	
	Method SetState(state:Int)
		GameState = state
	End
	
	Method DrawTitleScreen(canvas:Canvas)
		Asteroid.RenderAll(canvas)
		canvas.DrawText("MONKEYROIDS: RETURN OF THE CHIMP!", SCREEN_WIDTH / 2, (SCREEN_HEIGHT / 2) - 20, 0.5, 0.5)
		canvas.DrawText("BEST SCORE: " + bestScore, (SCREEN_WIDTH / 2), (SCREEN_HEIGHT / 2), 0.5, 0.5)
		canvas.DrawText("PRESS <SPACE> TO PLAY", SCREEN_WIDTH / 2, (SCREEN_HEIGHT / 2) + 20, 0.5, 0.5)
	End
	
	Method DrawHUD:Void(canvas:Canvas)
		canvas.DrawText("LEVEL: " + level, 0, 0)
		canvas.DrawText("SCORE: " + score, SCREEN_WIDTH, 0, 1, 0)
		canvas.DrawText("BULLETS#: " + Bullet.list.Count(), SCREEN_WIDTH/2, 0, 0.5, 0)
		canvas.DrawText("ASTEROIDS#: " + Asteroid.list.Count(), SCREEN_WIDTH/2, 20, 0.5, 0)
	End
	
	Method DrawGame(canvas:Canvas)
		player.Render(canvas)
		Bullet.RenderAll(canvas)
		Asteroid.RenderAll(canvas)
		DrawHUD(canvas)
	End
	
	Method DrawGameOver(canvas:Canvas)
		Asteroid.RenderAll(canvas)
		canvas.DrawText("GAME OVER!", SCREEN_WIDTH / 2, SCREEN_HEIGHT / 2 - 20, 0.5, 0.5)
		canvas.DrawText("SCORE: " + score, SCREEN_WIDTH / 2, SCREEN_HEIGHT / 2, 0.5, 0.5)
		canvas.DrawText("BEST SCORE: " + Self.bestScore, SCREEN_WIDTH / 2, SCREEN_HEIGHT / 2 + 20, 0.5, 0.5)
		canvas.DrawText("PRESS <SPACE> TO RETURN TO THE TITLE SCREEN", SCREEN_WIDTH / 2, SCREEN_HEIGHT / 2 + 40, 0.5, 0.5)
	End

	Method CheckCollisions()
		Local asteroidToRemove:Asteroid
		
		Local ait := Asteroid.list.All()		
		While Not ait.AtEnd
			Local a:Asteroid = ait.Current
			If Not a.dead And Dist(player.x, player.y, a.x, a.y) <= a.avgrad
				player.shield -= 10
				a.dead = True
			End
			For Local b:Bullet = Eachin Bullet.list
				If a <> Null And Not a.dead
					If Dist(b.x, b.y, a.x, a.y) <= a.avgrad
						a.life -= 1
						b.life = 0
					End
					If a.life <= 0
						a.dead = True
					End
				End
			Next
			If a.dead
				ait.Erase()
				asteroidToRemove = a
			Else
				ait.Bump()
			End

		End
		
		If asteroidToRemove <> Null
			If asteroidToRemove.size > 1
				score += 5
				For Local t:Int = 1 To 2
					New Asteroid(asteroidToRemove.x, asteroidToRemove.y, Rnd(-5, 5), Rnd(-5, 5), asteroidToRemove.size - 1)
				Next
			End
		End
		
		If Asteroid.list.Count() = 0
			level += 1
			asteroidsNum += 1
			asteroidsSize += 1
			CreateAsteroids(asteroidsNum, asteroidsSize)
		End
	End
	
	Method Reset()
		Bullet.list.Clear()
		Asteroid.list.Clear()
		level = 1
		asteroidsNum = 2
		asteroidsSize = 3
		score = 0
		player = New Player(SCREEN_WIDTH / 2, SCREEN_HEIGHT / 2)
		CreateAsteroids(asteroidsNum, asteroidsSize)	
	End
	
	Method CreateAsteroids:Void(num:Int, size:Int)
		Asteroid.list.Clear()
		Bullet.list.Clear()
		Local tx:Float
		Local ty:Float
		For Local t:Int = 1 To num
			Repeat
				tx = Rnd(640)
				ty = Rnd(480)
			Until (tx < 280 Or tx > 360) And (ty < 200 Or ty > 280)
			New Asteroid(tx, ty, Rnd(-2, 2), Rnd(-2, 2), size + Rnd(1))
		Next
	End
End

Class Sprite
	Field dead:Bool
	Field xv:Float, yv:Float
	Field x:Float, y:Float
	Field rotation:Float
	Field canvas:Canvas
End

Class Asteroid Extends Sprite
	Global list:List<Asteroid> = New List<Asteroid>
	Field ang:Float, angvel:Float
	Field rad:Float[] = New Float[9]
	Field avgrad:Float
	Field size:Int
	Field life:Int
	Field cr:Float, cg:Float, cb:Float
	
	Method New(x:Float, y:Float, xv:Float, yv:Float ,size:Int)
		Self.x = x 
		Self.y = y 
		Self.xv = xv 
		Self.yv = yv 
		Self.ang = Rnd(360)
		Self.angvel = Rnd(-6, 6)
		Self.size = size
		Self.life = size
		Self.cr = 1
		Self.cg = 1
		Self.cb = 1
		' Create "Rockiness"
		Self.avgrad = 0
		For Local t:Int = 0 To 7
			Self.rad[t] = size * 8.0 + Rnd(-size * 4.0, size * 4.0)
			Self.avgrad = Self.avgrad + Self.rad[t]
		Next
		Self.avgrad = Self.avgrad /6.0
		Self.rad[8] = Self.rad[0]
		Self.dead = False
		
		list.AddLast(self)
	End
	
	Function RenderAll:Void(canvas:Canvas)
		If Not list Return
		For Local a:Asteroid = Eachin list
			a.Render(canvas)
		Next		
	End
	
	Function UpdateAll:Void()
		If Not list Return
		For Local a:Asteroid = Eachin list
			a.Update()		
		Next
	End
	
	Method Update:Void()
		'If dead Return
		Self.x += Self.xv * Delta.delta 
		Self.y += Self.yv * Delta.delta 
		Self.rotation += Self.angvel * Delta.delta
		
		If Self.x < -Self.avgrad  Then Self.x = Self.x + SCREEN_WIDTH + Self.avgrad *2
		If Self.x > SCREEN_WIDTH + Self.avgrad Then Self.x = Self.x - SCREEN_WIDTH - Self.avgrad *2
		If Self.y < -Self.avgrad  Then Self.y = Self.y + SCREEN_HEIGHT + Self.avgrad *2
		If Self.y > SCREEN_HEIGHT + Self.avgrad Then Self.y = Self.y - SCREEN_HEIGHT - Self.avgrad *2
	End
	
	Method Render:Void(canvas:Canvas)
		'If dead Return
		Local tmul:Float = 360.0 / 8.0
		canvas.Color = New Color( cr, cg, cb)
		For Local t:Int = 0 To 7
			local x1:Float = x - (Sin(ToRadians(rotation + (t) * tmul)) * rad[t])
			Local y1:Float = y - (Cos(ToRadians(rotation + (t) * tmul)) * rad[t])
			Local x2:Float = x - (Sin(ToRadians(rotation + (t + 1) * tmul))* rad[t + 1])
			Local y2:Float = y - (Cos(ToRadians(rotation + (t + 1) * tmul)) * rad[t + 1])
			canvas.DrawLine(x1, y1, x2, y2)
		Next
	End
End

Class Bullet Extends Sprite
	Global list:List<Bullet> = New List<Bullet>
	Field life:Float
	Field cr:Float, cg:Float, cb:Float
	
	Method New(x:Float, y:Float, xv:Float, yv:Float, life:Float, cr:Float, cg:Float, cb:Float)
		Self.x = x
		Self.y = y
		Self.xv = xv
		Self.yv = yv
		Self.life = life
		Self.cr = cr
		Self.cg = cg
		Self.cb = cb
		
		list.AddLast(Self)
	End
	
	Function UpdateAll:Void()
		If Not list Return
		
		Local it := list.All()
		While Not it.AtEnd
			Local b:Bullet = it.Current
			b.Update()
			If b.life <= 0
				it.Erase()
			Else
				it.Bump()
			End
		End
	End
	
	Method Update:Void()
		If dead Return
		x += xv * Delta.delta
		y += yv * Delta.delta
		
		life -= Delta.delta
		If x < 0 x = SCREEN_WIDTH
		If x > SCREEN_WIDTH x = 0
		If y < 0 y = SCREEN_HEIGHT
		If y > SCREEN_HEIGHT y = 0
	End
	
	Function RenderAll:Void(canvas:Canvas)
		If Not list Return
		For Local b:Bullet = Eachin list
			b.Render(canvas)
		Next		
	End
	
	Method Render:Void(canvas:Canvas)
		If dead Return
		Local tmul:Float
		If life <= 25.0
			tmul = life / 25.0
		Else
			tmul = 1.0
		End
		canvas.Color = New Color( cr * tmul, cg * tmul, cb * tmul )
		canvas.DrawLine(x, y, x + xv, y + yv)
	End
End


Class Player Extends Sprite
	Field angVel:Float
	Field velmul:Float
	Field vel:Float
	Field acc:Float
	Field drag:Float
	Field xv:Float, yv:Float
	Field xa:Float, ya:Float
	Field firedel:Float
	
	Field shipAngvel:Float
	Field shipAcc:Float
	Field shipVelmul:Float
	Field shipFiredel:Float
	
	Field shield:Float
	
	Method New(x:Float, y:Float)
		Self.x = x
		Self.y = y
		Self.shipAngvel = 6
		Self.shipAcc = 0.16
		Self.shipVelmul = -0.0005
		Self.shipFiredel = 5
		Self.shield = 100
	End
	
	Method Controls()
		If Keyboard.KeyDown(Key.Up) Or Keyboard.KeyDown(Key.W)
			acc =  shipAcc
			drag = vel * shipVelmul
		Elseif Keyboard.KeyDown(Key.Down)  Or Keyboard.KeyDown(Key.S)
			drag = vel * shipVelmul * 50
		Else
			acc = 0
			drag = 0
		End
		
		If Keyboard.KeyDown(Key.Left) Or Keyboard.KeyDown(Key.A)
			rotation += shipAngvel * Delta.delta
		Elseif Keyboard.KeyDown(Key.Right) Or Keyboard.KeyDown(Key.D)
			rotation -= shipAngvel * Delta.delta
		End
		
		If Keyboard.KeyDown(Key.Space) And Self.firedel <= 0
			Local tang:Float = Rnd(-.01, .01)
			Local x1:Float = x - (Sin(ToRadians(rotation)) * 8)
			Local y1:Float = y - (Cos(ToRadians(rotation)) * 8)
			Local xv1:Float = xv - (Sin(ToRadians(rotation) + tang ) * 12)
			Local yv1:Float = yv - (Cos(ToRadians(rotation) + tang ) * 12)
			Local life:Int = 45		
			New Bullet(x1, y1, xv1, yv1, life, 1, 1, 0)
			firedel = shipFiredel
		End
		firedel -= Delta.delta
		
	End
	
	Method Update()
		Controls()
		xa = (drag * xv) - (Sin(ToRadians(rotation)) * acc)
		ya = (drag * yv) - (Cos(ToRadians(rotation)) * acc)
		xv += xa * Delta.delta
		yv += ya * Delta.delta
		x += xv * Delta.delta
		y += yv * Delta.delta
		vel = Dist(0, 0, xv, yv)
		
		If x < 0 x = SCREEN_WIDTH
		If x > SCREEN_WIDTH x = 0
		If y < 0 y = SCREEN_HEIGHT
		If y > SCREEN_HEIGHT y = 0
		
		If shield <= 0		
			If Game.score > Game.bestScore
				Game.bestScore = Game.score
			End
			Game.SetState(Game.STATE_GAME_OVER)
		End
	End
	
	Method Render(canvas:Canvas)

		Local x1:Float = x - (Sin(ToRadians(rotation)) * 10)
		Local y1:Float = y - (Cos(ToRadians(rotation)) * 10)
		
		Local x2:Float = x - (Sin(ToRadians(rotation + 140)) * 8)
		Local y2:Float = y - (Cos(ToRadians(rotation + 140)) * 8)
		
		Local x3:Float = x - (Sin(ToRadians(rotation - 140)) * 8)
		Local y3:Float = y - (Cos(ToRadians(rotation - 140)) * 8)
		
		canvas.Color = Color.White		
		canvas.DrawLine(x1, y1, x2, y2)
		canvas.DrawLine(x2, y2, x3, y3)
		canvas.DrawLine(x3, y3, x1, y1)
		
		canvas.Alpha = .5
		If shield < 50 Then canvas.Color = New Color(1, 0, 0)
		canvas.DrawRect(10, SCREEN_HEIGHT - 15, Self.shield, 10)
		canvas.Alpha = 1

	End
End

'TODO: Delta timing class
Class Delta
	Global delta:Float = 0.5
End

Global Game:MyWindow

Function Main()
	New AppInstance
	Game = New MyWindow("Monkeyroids: Return of the Chimp!", SCREEN_WIDTH, SCREEN_HEIGHT)
	App.Run()
End

Function Dist:Float(x1:Float, y1:Float, x2:Float, y2:Float)
	Return Sqrt(Pow((x1 - x2), 2) + Pow((y1 - y2), 2))
End

Function ToDegrees:Float(rad:Float)
	Return rad * 180.0 / PI
End

Function ToRadians:Float(degree:Float)
	Return degree * PI / 180.0
End
