/*
  Name       : The following code generates the permutations of first 'N' natural nos.
  Description: The value of 'N' can be set through #define N.
               The permutation are displayed in lexical order, smallest to largest, with appropriate signs
*/

#include<iostream>
#include<conio.h>
using namespace std;

//Function to calculate the factorial of a number
long int fact(int size)
{
	int i;
	long int temp =1;

	if (size<=1)
	{
		return 1;
	}
	else
	{
		for(i=size;i>0;i--)
			temp*=i;
	}

	return temp;

}


//Function to display the permutations.
void Permutations(int N)
{
    //Flag to indicate the sign
    signed short int Toggle_Flag = 1;
	
    //To keep track of when to change the sign.
    //Sign reverses when Toggle_Flag_Change_Condition = 0
    unsigned short int Toggle_Flag_Change_Condition =0;

    //Loop variables
    short int i =0;
    short int j =0;
    short int k =0;
	
    //Iterations
    long int Loops = fact(N);
	
    //Array of pointers to hold the digits
    int **Index_Nos_ptr = new int*[N];
	
    //Repetition of each digit (Master copy)
    int *Digit_Rep_Master = new int[N];
	
    //Repetition of each digit (Local copy)
    int *Digit_Rep_Local = new int[N];

    //Index for Index_Nos_ptr
    int *Element_Num = new int[N];
	

    //Initialization
    for(i=0;i<N;i++)
    {
		//Allocate memory to hold the subsequent digits in the form of a LUT
                //For N = N, memory required for LUT = N(N+1)/2 		
		Index_Nos_ptr[i] = new int[N-i];
		
		//Initialise the repetition value of each digit (Master and Local)
		//Each digit repeats for (i-1)!, where 1 is the position of the digit
		Digit_Rep_Local[i] = Digit_Rep_Master[i] = fact(N-i-1);

		//Initialise index values to access the arrays
		Element_Num[i] = N-i-1;
		
		//Initialise the arrays with the required digits
		for(j=0;j<(N-i);j++)
		{
			*(Index_Nos_ptr[i] +j) = N-j-1;
		}

    }//end of for()
	

    //Start with iteration
    while(Loops>0)
    {
		
		Loops--;
		
		cout<<"Perm: [";
		for(i=0;i<N;i++)
		{	
		
			//Print from MSD to LSD
			cout<<" "<<*(Index_Nos_ptr[i] + Element_Num[i]);
				
			//Decrement the repetition count for each digit
			Digit_Rep_Local[i]--;
				
			//If repetition count has reached 0...
			if(Digit_Rep_Local[i] <=0 )
			{
			
				//Refill the repitition factor
				Digit_Rep_Local[i] = Digit_Rep_Master[i];
						
				//And the index to access the required digit is also 0...
				if(Element_Num[i] <=0 && i!=0)
				{
					
					//Reset the index
					Element_Num[i] = N-i-1;			

							
					//Update the numbers held un Index_Nos_ptr[]					
					for(j=0,k=0;j<=N-i;j++)
					{
						//Exclude the preceeding digit(from the previous array) already printed.
						if(j!=Element_Num[i-1])
						{	
							*(Index_Nos_ptr[i]+k)= *(Index_Nos_ptr[i-1]+j);
							k++;
						}
					}
				}
				//If the index is not 0...
				else
				{
					//Decrement the index value so as to print the appropriate digit
					//in the same array
					Element_Num[i]--;
							
				}//end of if-else

			}//end of if()

		}//end of for()

		//Print the sign.
		cout<<"]  Sign: "<<Toggle_Flag<<"\n";	
			
		if(Toggle_Flag_Change_Condition > 0)
		{
			Toggle_Flag_Change_Condition--;
		}
		else
		{
			//Update the sign value.
			Toggle_Flag=-Toggle_Flag;
				
			//Reset Toggle_Flag_Change_Condition
			Toggle_Flag_Change_Condition =1;
			
		}//end of if-else
		
	}//end of while()	

}//end of Permutations()

int main()
{
    Permutations(4);
   	getch();
	return 0;
}
