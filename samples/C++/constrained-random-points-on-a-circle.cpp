#include <windows.h>
#include <list>
#include <iostream>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class point
{
public:
    int x, y;
    point()                  { x = y = 0; }
    point( int a, int b )    { x = a; y = b; }
    void set( int a, int b ) { x = a; y = b; }
};
//--------------------------------------------------------------------------------------------------
class rndCircle
{
public:
    void draw()
    {
	createPoints();
	drawPoints();
    }

private:
    void createPoints()
    {
	point pt;
	for( int x = 0; x < 200; x++ )
	{
	    int a, b, c;
	    while( true )
	    {
		a = rand() % 31 - 15;
		b = rand() % 31 - 15;
		c = a * a + b * b;
		if( c >= 100 && c <= 225 ) break;
	    }
	    pt.set( a, b );
	    _ptList.push_back( pt );
	}
    }

    void drawPoints()
    {
	HDC dc = GetDC( GetConsoleWindow() );
	for( list<point>::iterator it = _ptList.begin(); it != _ptList.end(); it++ )
	    SetPixel( dc, 300 + 10 * ( *it ).x, 300 + 10 * ( *it ).y, RGB( 255, 255, 0 ) );
    }

    list<point> _ptList;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );
    srand( GetTickCount() );
    rndCircle c;
    c.draw();
    system( "pause" );
    return 0;
}
//--------------------------------------------------------------------------------------------------
