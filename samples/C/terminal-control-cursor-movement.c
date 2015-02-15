/*Abhishek Ghosh, 7th November 2013, Rotterdam*/
#include<conio.h>
#include<dos.h>

char *strings[] = {"The cursor will move one position to the left",
  		   "The cursor will move one position to the right",
 		   "The cursor will move vetically up one line",
 		   "The cursor will move vertically down one line",
 		   "The cursor will move to the beginning of the line",
 		   "The cursor will move to the end of the line",
 		   "The cursor will move to the top left corner of the screen",
 		   "The cursor will move to the bottom right corner of the screen"};
 		
int main()
{
	int i,j,MAXROW,MAXCOL;
	struct text_info tInfo;
	gettextinfo(&tInfo);
	MAXROW = tInfo.screenheight;
	MAXCOL = tInfo.screenwidth;
	
	clrscr();
	cprintf("This is a demonstration of cursor control using gotoxy(). Press any key to continue.");
	getch();
	
	for(i=0;i<8;i++)
	{
		clrscr();
		gotoxy(5,MAXROW/2);
		
		cprintf("%s",strings[i]);
		getch();
		
		switch(i){
			case 0:gotoxy(wherex()-1,wherey());
			break;
			case 1:gotoxy(wherex()+1,wherey());
			break;
			case 2:gotoxy(wherex(),wherey()-1);
			break;
			case 3:gotoxy(wherex(),wherey()+1);
			break;
			case 4:for(j=0;j<strlen(strings[i]);j++){
				   gotoxy(wherex()-1,wherey());
				   delay(100);
			       }
			break;
			case 5:gotoxy(wherex()-strlen(strings[i]),wherey());
			       for(j=0;j<strlen(strings[i]);j++){
				   gotoxy(wherex()+1,wherey());
				   delay(100);
			       }
			break;
			case 6:while(wherex()!=1)
			       {
				     gotoxy(wherex()-1,wherey());
				     delay(100);
		               }
			       while(wherey()!=1)
			       {
			             gotoxy(wherex(),wherey()-1);
			             delay(100);
			       }
			break;
			case 7:while(wherex()!=MAXCOL)
			       {
				     gotoxy(wherex()+1,wherey());
				     delay(100);
			       }
			       while(wherey()!=MAXROW)
			       {
				     gotoxy(wherex(),wherey()+1);
				     delay(100);
			       }
			break;
			};
			getch();
	}
	
	clrscr();
	cprintf("End of demonstration.");
	getch();
	return 0;
}
