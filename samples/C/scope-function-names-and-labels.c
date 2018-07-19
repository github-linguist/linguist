/*Abhishek Ghosh, 8th November 2013, Rotterdam*/
#include<stdio.h>

#define sqr(x) x*x
#define greet printf("\nHello There !");

int twice(int x)
{
	return 2*x;
}

int main()
{
	int x;
	printf("\nThis will demonstrate function and label scopes.");
	printf("\nAll output is happening throung printf(), a function declared in the header file stdio.h, which is external to this program.");
	printf("\nEnter a number : ");
	scanf("%d",&x);
	
	switch(x%2){
		default:printf("\nCase labels in switch statements have scope local to the switch block.");
		case 0: printf("\nYou entered an even number.");
				  printf("\nIt's square is %d, which was computed by a macro. It has global scope within the program file.",sqr(x));
				  break;
		case 1: printf("\nYou entered an odd number.");
				  goto sayhello;
		jumpin: printf("\n2 times %d is %d, which was computed by a function defined in this file. It has global scope within the program file.",x,twice(x));
				  printf("\nSince you jumped in, you will now be greeted, again !");
	 sayhello: greet
	 			  if(x==-1)goto scram;
	 			  break;
	 };
	
	 printf("\nWe now come to goto, it's extremely powerful but it's also prone to misuse. It's use is discouraged and it wasn't even adopted by Java and later languages.");
	 				
	 if(x!=-1){
		 x = -1;     /*To break goto infinite loop.*/
	 	 goto jumpin;
	 	 }
	
	 scram: printf("\nIf you are trying to figure out what happened, you now understand goto.");
	 return 0;
}
	 			
