#include <SDL/SDL.h>
#ifdef WITH_CAIRO
#include <cairo.h>
#else
#include <SDL/sge.h>
#endif
#include <cairo.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#ifdef WITH_CAIRO
#define PI 3.1415926535
#endif

#define SIZE           800   // determines size of window
#define SCALE          5     // determines how quickly branches shrink (higher value means faster shrinking)
#define BRANCHES       14    // number of branches
#define ROTATION_SCALE 0.75  // determines how slowly the angle between branches shrinks (higher value means slower shrinking)
#define INITIAL_LENGTH 50    // length of first branch

double rand_fl(){
  return (double)rand() / (double)RAND_MAX;
}

void draw_tree(SDL_Surface * surface, double offsetx, double offsety,
               double directionx, double directiony, double size,
               double rotation, int depth) {
#ifdef WITH_CAIRO
  cairo_surface_t *surf = cairo_image_surface_create_for_data( surface->pixels,
                                                               CAIRO_FORMAT_RGB24,
							       surface->w, surface->h,
							       surface->pitch );
  cairo_t *ct = cairo_create(surf);

  cairo_set_line_width(ct, 1);
  cairo_set_source_rgba(ct, 0,0,0,1);
  cairo_move_to(ct, (int)offsetx, (int)offsety);
  cairo_line_to(ct, (int)(offsetx + directionx * size), (int)(offsety + directiony * size));
  cairo_stroke(ct);
#else
  sge_AALine(surface,
      (int)offsetx, (int)offsety,
      (int)(offsetx + directionx * size), (int)(offsety + directiony * size),
      SDL_MapRGB(surface->format, 0, 0, 0));
#endif
  if (depth > 0){
    // draw left branch
    draw_tree(surface,
        offsetx + directionx * size,
        offsety + directiony * size,
        directionx * cos(rotation) + directiony * sin(rotation),
        directionx * -sin(rotation) + directiony * cos(rotation),
        size * rand_fl() / SCALE + size * (SCALE - 1) / SCALE,
        rotation * ROTATION_SCALE,
        depth - 1);

    // draw right branch
    draw_tree(surface,
        offsetx + directionx * size,
        offsety + directiony * size,
        directionx * cos(-rotation) + directiony * sin(-rotation),
        directionx * -sin(-rotation) + directiony * cos(-rotation),
        size * rand_fl() / SCALE + size * (SCALE - 1) / SCALE,
        rotation * ROTATION_SCALE,
        depth - 1);
  }
}

void render(SDL_Surface * surface){
  SDL_FillRect(surface, NULL, SDL_MapRGB(surface->format, 255, 255, 255));
  draw_tree(surface,
      surface->w / 2.0,
      surface->h - 10.0,
      0.0, -1.0,
      INITIAL_LENGTH,
      PI / 8,
      BRANCHES);
  SDL_UpdateRect(surface, 0, 0, 0, 0);
}

int main(){
  SDL_Surface * screen;
  SDL_Event evt;

  SDL_Init(SDL_INIT_VIDEO);

  srand((unsigned)time(NULL));

  screen = SDL_SetVideoMode(SIZE, SIZE, 32, SDL_HWSURFACE);

  render(screen);
  while(1){
    if (SDL_PollEvent(&evt)){
      if(evt.type == SDL_QUIT) break;
    }
    SDL_Delay(1);
  }
  SDL_Quit();
  return 0;
}
