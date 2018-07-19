#include <stdio.h>
#include <stdlib.h>
#include <math.h>
/* #include "imglib.h" */

#define RED_C 0
#define GREEN_C 1
#define BLUE_C 2
#define GET_PIXEL(IMG, X, Y) ((IMG)->buf[ (Y) * (IMG)->width + (X) ])

int main(int argc, char **argv)
{
   image im1, im2;
   double totalDiff = 0.0;
   unsigned int x, y;

   if ( argc < 3 )
   {
      fprintf(stderr, "usage:\n%s FILE1 FILE2\n", argv[0]);
      exit(1);
   }
   im1 = read_image(argv[1]);
   if ( im1 == NULL ) exit(1);
   im2 = read_image(argv[2]);
   if ( im2 == NULL ) { free_img(im1); exit(1); }
   if ( (im1->width != im2->width) || (im1->height != im2->height) )
   {
      fprintf(stderr, "width/height of the images must match!\n");
   } else {
   /* BODY: the "real" part! */
      for(x=0; x < im1->width; x++)
      {
         for(y=0; y < im1->width; y++)
         {
           totalDiff += fabs( GET_PIXEL(im1, x, y)[RED_C] - GET_PIXEL(im2, x, y)[RED_C] ) / 255.0;
           totalDiff += fabs( GET_PIXEL(im1, x, y)[GREEN_C] - GET_PIXEL(im2, x, y)[GREEN_C] ) / 255.0;
           totalDiff += fabs( GET_PIXEL(im1, x, y)[BLUE_C] - GET_PIXEL(im2, x, y)[BLUE_C] ) / 255.0;
         }
      }
      printf("%lf\n", 100.0 * totalDiff / (double)(im1->width * im1->height * 3) );
   /* BODY ends */
   }
   free_img(im1);
   free_img(im2);
}
