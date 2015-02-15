#include<stdio.h>

int main()
{
	char ch, str[100];
	int i;
	
	do{
		printf("\nEnter the string :");
		fgets(str,100,stdin);
		for(i=0;str[i]!=00;i++)
		{
			if(str[i]=='#'||str[i]==';')
			{
				str[i]=00;
				break;
			}
		}
		printf("\nThe modified string is : %s",str);
		printf("\nDo you want to repeat (y/n): ");
		scanf("%c",&ch);
		fflush(stdin);
	}while(ch=='y'||ch=='Y');
	
	return 0;
}
