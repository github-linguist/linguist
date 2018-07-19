import std.stdio, std.random, sdl.SDL;

void main() {
  SDL_Init(SDL_INIT_TIMER | SDL_INIT_VIDEO);
  auto surface = SDL_SetVideoMode(320,240,8, SDL_DOUBLEBUF|SDL_HWSURFACE);

  uint frameNumber, totalTime, lastTime;
  while (true) {
    SDL_LockSurface(surface);
    foreach (i; 0 .. surface.w * surface.h)
      (cast(ubyte*)surface.pixels)[i] = (uniform(0, 2) ? 255 : 0);
    SDL_UnlockSurface(surface);
    SDL_Flip(surface);
    frameNumber++;

    uint time = SDL_GetTicks();
    totalTime += time - lastTime;
    if (totalTime > 1000) {
      writeln("FPS: ", frameNumber / (totalTime / 1000.0));
      totalTime = frameNumber = 0;
    }
    lastTime = time;
  }
}
