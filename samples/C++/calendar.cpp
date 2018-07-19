#include <windows.h>
#include <iostream>

//--------------------------------------------------------------------------------------------------
using namespace std;


//--------------------------------------------------------------------------------------------------
class calender
{
public:
    void drawCalender( int y )
    {
	year = y;
	for( int i = 0; i < 12; i++ )
	    firstdays[i] = getfirstday( i );

	isleapyear();
	build();
    }

private:
    void isleapyear()
    {
	isleap = false;

	if( !( year % 4 ) )
	{
	    if( year % 100 ) isleap = true;
	    else if( !( year % 400 ) ) isleap = true;
	}
    }

    int getfirstday( int m )
    {
	int y = year;

	int f = y + 1 + 3 * m - 1;
	m++;
	if( m < 3 ) y--;
	else f -= int( .4 * m + 2.3 );

	f += int( y / 4 ) - int( ( y / 100 + 1 ) * 0.75 );
	f %= 7;

	return f;
    }

    void build()
    {
	int days[] = { 31, isleap ? 29 : 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 };
	int lc = 0, lco = 0, ystr = 7, start = 2, fd = 0, m = 0;
	HANDLE h = GetStdHandle( STD_OUTPUT_HANDLE );
	COORD pos = { 0, ystr };
	draw();

	for( int i = 0; i < 4; i++ )
	{
	    for( int j = 0; j < 3; j++ )
	    {
		int d = firstdays[fd++], dm = days[m++];
		pos.X = d * 3 + start;
		SetConsoleCursorPosition( h, pos );

		for( int dd = 0; dd < dm; dd++ )
		{
		    if( dd < 9 ) cout << 0 << dd + 1 << " ";
		    else cout << dd + 1 << " ";

		    pos.X += 3;
		    if( pos.X - start > 20 )
		    {
			pos.X = start; pos.Y++;
			SetConsoleCursorPosition( h, pos );
		    }
		}

		start += 23;
		pos.X = start; pos.Y = ystr;
		SetConsoleCursorPosition( h, pos );
	    }
	    ystr += 9; start = 2;
	    pos.Y = ystr;
	}
    }

    void draw()
    {
	system( "cls" );
	cout << "+--------------------------------------------------------------------+" << endl;
	cout << "|                              [SNOOPY]                              |" << endl;
	cout << "|                                                                    |" << endl;
	cout << "|                             == " << year << " ==                             |" << endl;
	cout << "+----------------------+----------------------+----------------------+" << endl;
	cout << "|       JANUARY        |       FEBRUARY       |         MARCH        |" << endl;
	cout << "| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "+----------------------+----------------------+----------------------+" << endl;
	cout << "|        APRIL         |          MAY         |         JUNE         |" << endl;
	cout << "| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "+----------------------+----------------------+----------------------+" << endl;
	cout << "|         JULY         |        AUGUST        |       SEPTEMBER      |" << endl;
	cout << "| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "+----------------------+----------------------+----------------------+" << endl;
	cout << "|        OCTOBER       |       NOVEMBER       |       DECEMBER       |" << endl;
	cout << "| SU MO TU WE TH FR SA | SU MO TU WE TH FR SA | SU MO TU WE TH FR SA |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "|                      |                      |                      |" << endl;
	cout << "+----------------------+----------------------+----------------------+" << endl;
    }

    int firstdays[12], year;
    bool isleap;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    int y;
    calender cal;

    while( true )
    {
	system( "cls" );
	cout << "Enter the year( yyyy ) --- ( 0 to quit ): ";
	cin >> y;
	if( !y ) return 0;

	cal.drawCalender( y );
	cout << endl << endl << endl << endl << endl << endl << endl << endl;

	system( "pause" );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------
