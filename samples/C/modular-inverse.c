#include<stdio.h>
int modularinverse( int a, int b){
	int c=b/a,x=0;
	do{
		if((a<b)||(a==1)) x=1;
		if((c*a)%b==1) x=c;
		else c++;
	} while(x==0);
	return x;
}
int main()
{
	int a,b;
	printf("Unesite brojeve a i b ");
	scanf("%d%d",&a,&b);
	printf("Modularni inverz brojeva %d i %d je %d ",a,b,modularinverse(a,b));
	return 0;
}
