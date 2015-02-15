declare
  [QTk] = {Link ['x-oz://system/wp/QTk.ozf']}

  Pi = 3.14159265

  class PendulumModel
     feat
	K
     attr
	angle
	velocity

     meth init(length:L       <= 1.0    %% meters
	       gravity:G      <= 9.81   %% m/s²
	       initialAngle:A <= Pi/2.) %% radians
	self.K = ~G / L
	angle := A
	velocity := 0.0
     end

     meth nextAngle(deltaT:DeltaTMS %% milliseconds
		    ?Angle)         %% radians
        DeltaT = {Int.toFloat DeltaTMS} / 1000.0 %% seconds
        Acceleration = self.K * {Sin @angle}
     in
        velocity := @velocity + Acceleration * DeltaT
        angle := @angle + @velocity * DeltaT
        Angle = @angle
     end
  end

  %% Animates a pendulum on a given canvas.
  class PendulumAnimation from Time.repeat
     feat
        Pend
        Rod
        Bob
        home:pos(x:160 y:50)
        length:140.0
	delay

     meth init(Pendulum Canvas delay:Delay <= 25) %% milliseconds
	self.Pend = Pendulum
	self.delay = Delay
	%% plate and pivot
        {Canvas create(line 0 self.home.y 320 self.home.y width:2 fill:grey50)}
        {Canvas create(oval 155 self.home.y-5 165 self.home.y+5 fill:grey50 outline:black)}
	%% the pendulum itself
	self.Rod = {Canvas create(line 1 1 1 1 width:3 fill:black handle:$)}
        self.Bob = {Canvas create(oval 1 1 2 2 fill:yellow outline:black handle:$)}
        %%
        {self setRepAll(action:Animate delay:Delay)}
     end

     meth Animate
	Theta = {self.Pend nextAngle(deltaT:self.delay $)}
	%% calculate x and y from angle
	X = self.home.x + {Float.toInt self.length * {Sin Theta}}
	Y = self.home.y + {Float.toInt self.length * {Cos Theta}}
     in
	%% update canvas
	try
	   {self.Rod setCoords(self.home.x self.home.y X Y)}
	   {self.Bob setCoords(X-15 Y-15 X+15 Y+15)}
	catch system(tk(alreadyClosed ...) ...) then skip end
     end
  end

  Pendulum = {New PendulumModel init}

  Canvas
  GUI = td(title:"Pendulum"
           canvas(width:320 height:210 handle:?Canvas)
           action:proc {$} {Animation stop} {Window close} end
          )
  Window = {QTk.build GUI}

  Animation = {New PendulumAnimation init(Pendulum Canvas)}
in
  {Window show}
  {Animation go}
