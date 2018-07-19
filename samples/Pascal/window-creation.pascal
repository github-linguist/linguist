Program WindowCreation_SDL;

{$linklib SDL}

uses
  SDL,
  SysUtils;

var
  screen: PSDL_Surface;

begin
  SDL_Init(SDL_INIT_VIDEO);
  screen := SDL_SetVideoMode( 800, 600, 16, (SDL_SWSURFACE or SDL_HWPALETTE) );
  sleep(2000);
  SDL_Quit;
end.
