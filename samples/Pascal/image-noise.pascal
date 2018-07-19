Program ImageNoise;

uses
  SDL;

var
  surface: PSDL_Surface;
  pixel: ^byte;
  frameNumber, totalTime, lastTime, time, i: longint;

begin
  frameNumber := 0;
  totalTime   := 0;
  lastTime    := 0;
  randomize;
  SDL_Init(SDL_INIT_TIMER or SDL_INIT_VIDEO);
  surface := SDL_SetVideoMode(320, 240, 8, SDL_DOUBLEBUF or SDL_HWSURFACE);

  while (true) do
  begin
    pixel := surface^.pixels;
    SDL_LockSurface(surface);
    for i := 1 to (surface^.w * surface^.h) do
    begin
      pixel^ := random(2)*255;
      inc(pixel);
    end;
    SDL_UnlockSurface(surface);
    SDL_Flip(surface);
    inc (frameNumber);

    time := SDL_GetTicks();
    totalTime := totalTime + time - lastTime;
    if (totalTime > 1000) then
    begin
      writeln('FPS: ', frameNumber / (totalTime / 1000.0):5:1);
      frameNumber := 0;
      totalTime := 0;
    end;
    lastTime := time;
  end;
end.
