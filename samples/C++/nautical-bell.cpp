#include <iostream>
#include <string>
#include <windows.h>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class bells
{
public:
    void start()
    {
	watch[0] = "Middle"; watch[1] = "Morning"; watch[2] = "Forenoon"; watch[3] = "Afternoon"; watch[4] = "Dog"; watch[5] =  "First";
	count[0] = "One"; count[1] = "Two"; count[2] = "Three"; count[3] = "Four"; count[4] = "Five"; count[5] = "Six"; count[6] = "Seven"; count[7] = "Eight";
	_inst = this; CreateThread( NULL, 0, bell, NULL, 0, NULL );
    }
private:
    static DWORD WINAPI bell( LPVOID p )
    {
	DWORD wait = _inst->waitTime();
	while( true )
	{
	    Sleep( wait );
	    _inst->playBell();
	    wait = _inst->waitTime();
	}
	return 0;
    }

    DWORD waitTime()
    {
	GetLocalTime( &st );
	int m = st.wMinute >= 30 ? st.wMinute - 30 : st.wMinute;
	return( 1800000 - ( ( m * 60 + st.wSecond ) * 1000 + st.wMilliseconds ) );
    }

    void playBell()
    {
	GetLocalTime( &st );
	int b = ( 2 * st.wHour + st.wMinute / 30 ) % 8; b = b == 0 ? 8 : b;
	int w = ( 60 * st.wHour + st.wMinute );
	if( w < 1 ) w = 5; else w = ( w - 1 ) / 240;
	char hr[32]; wsprintf( hr, "%.2d:%.2d", st.wHour, st.wMinute );

	cout << hr << " - " << watch[w] << " watch - " << count[b - 1] << " Bell";
	if( b > 1 ) cout << "s"; else cout << " "; cout << " Gone." << endl;

	for( int x = 0, c = 1; x < b; x++, c++ )
	{
	    cout << "\7"; Sleep( 500 );
	    if( !( c % 2 ) ) Sleep( 300 );
	}
    }

    SYSTEMTIME st;
    string watch[7], count[8];
    static bells* _inst;
};
//--------------------------------------------------------------------------------------------------
bells* bells::_inst = 0;
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    bells b; b.start();
    while( 1 ); // <- runs forever!
    return 0;
}
//--------------------------------------------------------------------------------------------------
