/*A C calculator...of sorts*/
/* An attempt at a C calculator from stuff read so far */

#include<stdio.h>

/* function for addition */

int add(int input1, int input2)
{
  int result;
  
  result = input1 + input2;
  
  return result;
}

/* function for multiplication */

int multi(int input1, int input2)
{
  int result;
  
  result = input1 * input2;

  return result;
}

/* function for subtraction */

int sub(int input1, int input2)
{
  int result;
  
  result = input1 - input2;

  return result;
}

/* division function */

float div(float input1, float input2)
{
  float result;
  
  result = input1 / input2;

  return result;
}

int main()
{
  int a, b, output;
  float output2;
  char myinput;

  printf("Please enter a number\n");
  scanf("%d", &a);
  printf("Enter another number\n");
  scanf("%d", &b);
  printf("What calculation would you like to perform?\n");
  printf("a) addition\n");
  printf("b) mulitplication\n");
  printf("c) subtraction\n");
  printf("d) division\n");
  scanf(" %c", &myinput);

  /* switch statement to run certain calculations */
  switch(myinput)
  {
    case 'a':
      {
        printf("Adding the numbers entered...\n");
        output = add(a, b);
        printf("The sum of %d and %d is: %d\n", a, b, output);
        break;
      }
    case 'b':
      {
        printf("Multiplication chosen\n");
        output = multi(a, b);
        printf("Multiplying %d and %d equals %d\n", a, b , output);
        break;
      }
    case 'c':
      {
        printf("Subtracting %d from %d\n", a, b);
        output = sub(a, b);
        printf("%d minus %d is: %d\n", a, b, output);
        break;
      }
    case 'd':
      {
        printf("Divison program running...\n");
        output2 = div(a, b);
        printf("Division of %d by %d equals %f\n", a, b, output2);
        break;
      }
    default:
      {
        printf("Invalid entry\n");
        printf("Please run again\n");
      }
  }
  
  return 0;
}
