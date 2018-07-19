#include <stdlib.h>
#include <stdio.h>
int main(int argc, char** argv) {
   if (argc < 2) {
      printf("usage: identitymatrix <number of rows>\n");
      exit(EXIT_FAILURE);
   }
   signed int rowsize = atoi(argv[1]);
   if (rowsize < 0) {
      printf("Dimensions of matrix cannot be negative\n");
      exit(EXIT_FAILURE);
   }
   volatile int numElements = rowsize * rowsize;
   if (numElements < rowsize) {
      printf("Squaring %d caused result to overflow to %d.\n", rowsize, numElements);
      abort();
   }
   int** matrix = calloc(numElements, sizeof(int*));
   if (!matrix) {
      printf("Failed to allocate %d elements of %d bytes each\n", numElements, sizeof(int*));
      abort();
   }
   for (unsigned int row = 0;row < rowsize;row++) {
      matrix[row] = calloc(numElements, sizeof(int));
      if (!matrix[row]) {
         printf("Failed to allocate %d elements of %d bytes each\n", numElements, sizeof(int));
         abort();
      }
      matrix[row][row] = 1;
   }
   printf("Matrix is: \n");
   for (unsigned int row = 0;row < rowsize;row++) {
      for (unsigned int column = 0;column < rowsize;column++) {
         printf("%d ", matrix[row][column]);
      }
      printf("\n");
   }
}
