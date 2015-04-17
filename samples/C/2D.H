#ifndef __2DGFX
#define __2DGFX
// Includes
#include <stdio.h>
#include <math.h>
#include <conio.h>
#include <dpmi.h>

// Defines
#define VGAPIX(x,y) (vgabasemem + (x) + (y) * 320)

// Variables
char * vgabasemem;
DPMI_REGS regs;

// Drawing functions:
//void setvgabasemem(void);
void drw_chdis(int mode); // draw_func_change_display
void drw_pix(int x, int y, enum COLORS col);
void drw_line(int x0, int y0, int x1, int y1, enum COLORS col);
void drw_rectl(int x, int y, int w, int h, enum COLORS col);
void drw_rectf(int x, int y, int w, int h, enum COLORS col);
void drw_cirl(int x, int y, int rad, enum COLORS col);
void drw_tex(int x, int y, int w, int h, enum COLORS tex[]);
void 2D_init(void);
void 2D_exit(void);


#endif
