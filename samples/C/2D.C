#include "2D.h"
#include <math.h>

void set_vgabasemem(void)
{
    ULONG vgabase;
    SELECTOR tmp;
    asm mov [tmp], ds
    dpmi_get_sel_base(&vgabase, tmp);
    vgabasemem = (char *)(-vgabase + 0xa0000);
}

void drw_chdis(int mode) // change the display!
{
    regs.b.ah = 0x00;   // seet theh display moode
    regs.b.al = mode;  // change it to the mode like innit
    regs.h.flags = 0x72;// Set the dingoes kidneys out of FLAGS eh?
    regs.h.ss = 0;     // Like, totally set the stack segment
    regs.h.sp = 0;     // Set tha stack pointaaaaahhhhh!!!
    dpmi_simulate_real_interrupt(0x10, &regs);
}

void drw_pix(int x, int y, enum COLORS col)
{
    *VGAPIX(x, y) = col;
}

void drw_line(int x0, int y0, int x1, int y1, enum COLORS col)
{
    // Going for the optimized version of bresenham's line algo.        
    int stp = (abs(y0 - y1) > abs(x0 - x1));
    int tmp, dx, dy, err, yi, i, j; // yi = y excrement
    if (stp) {
        // swappity swap
        tmp = y0;
        y0 = x0;
        x0 = tmp;
        
        tmp = y1;
        y1 = x1;
        x1 = tmp;
    }
    // AAAAND NOW WE MUST DO ZEES AGAIN :(
    // I'm sure there was a func somewhere that does this? :P
    if (x0 > x1) {
        tmp = x0;
        x0 = x1;
        x1 = tmp;
        
        tmp = y0;
        y0 = y1;
        y1 = tmp;
    }
    dx = (x1 - x0);
    dy = (abs(y1 - y0));
    err = (dx / 2);

    if (y0 < y1)
        yi = 1;
    else
        yi = -1;
    j = y0;
    for (i = x0; i < x1; i++)
    {
        if (stp)
            *VGAPIX(j, i) = col;
        else
            *VGAPIX(i, j) = col;

        err -= dy;
        if (err < 0) {
            j += yi;
            err += dx;
        }
    }
}

void drw_rectl(int x, int y, int w, int h, enum COLORS col)
{
    drw_line(x, y, x+w, y, col);
    drw_line(x+w, y, x+w, y+h, col);

    drw_line(x, y, x, y+h, col);
    drw_line(x, y+h, x+w+1, y+h, col);
}

void drw_rectf(int x, int y, int w, int h, enum COLORS col)
{
    int i, j;
    for (j = y; j < x+h; j++) {
        for (i = x; i < y+w; i++) {
            *VGAPIX(i, j) = col;
        }
    }
}

void drw_circl(int x, int y, int rad, enum COLORS col)
{
    int mang, i; // max angle, haha
    int px, py;
    mang = 360; // Yeah yeah I'll switch to rad later
    for (i = 0; i <= mang; i++)
    {   
        px = cos(i)*rad + x; // + px; // causes some really cools effects! :D
        py = sin(i)*rad + y; // + py;
        *VGAPIX(px, py) = col;
    }
}

void drw_tex(int x, int y, int w, int h, enum COLORS tex[])
{   // i*w+j
    int i, j;
    for (i = 0; i < w; i++)
    {
        for (j = 0; j < h; j++)
        {
            *VGAPIX(x+i, y+j) = tex[j*w+i];
        }
    }
}

void 2D_init(void)
{
    set_vgabasemem();
    drw_chdis(0x13);
}

void 2D_exit(void)
{
    drw_chdis(3);
}
/*
int main()
{
    set_vgabasemem();
    drw_chdis(0x13);

    while(!kbhit()) {
        if ((getch()) == 0x1b) // escape
            break;
    }
    drw_chdis(3);
    return 0;
}
*/
