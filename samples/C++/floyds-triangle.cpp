#include <windows.h>
#include <sstream>
#include <iostream>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class floyds_tri
{
public:
    floyds_tri()  { lastLineLen = 0; }
    ~floyds_tri() { killArray(); }

    void create( int rows )
    {
	_rows = rows;
	calculateLastLineLen();
	display();
    }

private:
    void killArray()
    {
	if( lastLineLen )
	    delete [] lastLineLen;
    }

    void calculateLastLineLen()
    {
	killArray();
	lastLineLen = new BYTE[_rows];

	int s = 1 + ( _rows * ( _rows - 1 ) ) / 2;

	for( int x = s, ix = 0; x < s + _rows; x++, ix++ )
	{
	    ostringstream cvr;
	    cvr << x;
	    lastLineLen[ix] = static_cast<BYTE>( cvr.str().size() );
	}
    }

    void display()
    {
	cout << endl << "Floyd\'s Triangle - " << _rows << " rows" << endl << "===============================================" << endl;
	int number = 1;
	for( int r = 0; r < _rows; r++ )
	{
	    for( int c = 0; c <= r; c++ )
	    {
		ostringstream cvr;
		cvr << number++;
		string str = cvr.str();
		while( str.length() < lastLineLen[c] )
		    str = " " + str;
		cout << str << " ";
	    }
	    cout << endl;
	}
    }

    int _rows;
    BYTE* lastLineLen;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    floyds_tri t;
    int s;
    while( true )
    {
	cout << "Enter the size of the triangle ( 0 to QUIT ): "; cin >> s;
	if( !s ) return 0;
	if( s > 0 ) t.create( s );

	cout << endl << endl;
	system( "pause" );
    }

    return 0;
}
//--------------------------------------------------------------------------------------------------
