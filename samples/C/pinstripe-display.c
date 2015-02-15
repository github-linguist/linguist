/*Abhishek Ghosh, 6th November 2013, Rotterdam*/
#include<graphics.h>
#include<conio.h>

#define sections 4

int main()
{
	int d=DETECT,m,maxX,maxY,x,y,increment=1;
	initgraph(&d,&m,"c:/turboc3/bgi");

	maxX = getmaxx();
	maxY = getmaxy();

	for(y=0;y<maxY;y+=maxY/sections)
	{
		for(x=0;x<maxX;x+=increment)
		{
			setfillstyle(SOLID_FILL,(x/increment)%2==0?BLACK:WHITE); //The only line which differs
			bar(x,y,x+increment,y+maxY/sections);
		}
		increment++;
	}

	getch();
	closegraph();
	return 0;
}
