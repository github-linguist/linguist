#include <windows.h>
#include <iostream>

//-----------------------------------------------------------------------------------------
using namespace std;

//-----------------------------------------------------------------------------------------
const int BMP_SIZE = 800, NORTH = 1, EAST = 2, SOUTH = 4, WEST = 8, LEN = 1;

//-----------------------------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ), brush( NULL ), clr( 0 ), wid( 1 ) {}
    ~myBitmap()
    {
	DeleteObject( pen ); DeleteObject( brush );
	DeleteDC( hdc ); DeleteObject( bmp );
    }

    bool create( int w, int h )
    {
	BITMAPINFO bi;
	ZeroMemory( &bi, sizeof( bi ) );
	bi.bmiHeader.biSize        = sizeof( bi.bmiHeader );
	bi.bmiHeader.biBitCount    = sizeof( DWORD ) * 8;
	bi.bmiHeader.biCompression = BI_RGB;
	bi.bmiHeader.biPlanes      = 1;
	bi.bmiHeader.biWidth       =  w;
	bi.bmiHeader.biHeight      = -h;

	HDC dc = GetDC( GetConsoleWindow() );
	bmp = CreateDIBSection( dc, &bi, DIB_RGB_COLORS, &pBits, NULL, 0 );
	if( !bmp ) return false;

	hdc = CreateCompatibleDC( dc );
	SelectObject( hdc, bmp );
	ReleaseDC( GetConsoleWindow(), dc );

	width = w; height = h;
	return true;
    }

    void clear( BYTE clr = 0 )
    {
	memset( pBits, clr, width * height * sizeof( DWORD ) );
    }

    void setBrushColor( DWORD bClr )
    {
	if( brush ) DeleteObject( brush );
	brush = CreateSolidBrush( bClr );
	SelectObject( hdc, brush );
    }

    void setPenColor( DWORD c )
    {
	clr = c; createPen();
    }

    void setPenWidth( int w )
    {
	wid = w; createPen();
    }

    void saveBitmap( string path )
     {
	BITMAPFILEHEADER fileheader;
	BITMAPINFO       infoheader;
	BITMAP           bitmap;
	DWORD            wb;

	GetObject( bmp, sizeof( bitmap ), &bitmap );
	DWORD* dwpBits = new DWORD[bitmap.bmWidth * bitmap.bmHeight];

	ZeroMemory( dwpBits, bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD ) );
	ZeroMemory( &infoheader, sizeof( BITMAPINFO ) );
	ZeroMemory( &fileheader, sizeof( BITMAPFILEHEADER ) );

	infoheader.bmiHeader.biBitCount = sizeof( DWORD ) * 8;
	infoheader.bmiHeader.biCompression = BI_RGB;
	infoheader.bmiHeader.biPlanes = 1;
	infoheader.bmiHeader.biSize = sizeof( infoheader.bmiHeader );
	infoheader.bmiHeader.biHeight = bitmap.bmHeight;
	infoheader.bmiHeader.biWidth = bitmap.bmWidth;
	infoheader.bmiHeader.biSizeImage = bitmap.bmWidth * bitmap.bmHeight * sizeof( DWORD );

	fileheader.bfType    = 0x4D42;
	fileheader.bfOffBits = sizeof( infoheader.bmiHeader ) + sizeof( BITMAPFILEHEADER );
	fileheader.bfSize    = fileheader.bfOffBits + infoheader.bmiHeader.biSizeImage;

	GetDIBits( hdc, bmp, 0, height, ( LPVOID )dwpBits, &infoheader, DIB_RGB_COLORS );

	HANDLE file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
	WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
	WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
	WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
	CloseHandle( file );

	delete [] dwpBits;
    }

    HDC getDC() const     { return hdc; }
    int getWidth() const  { return width; }
    int getHeight() const { return height; }

private:
    void createPen()
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, wid, clr );
	SelectObject( hdc, pen );
    }

    HBITMAP bmp;
    HDC     hdc;
    HPEN    pen;
    HBRUSH  brush;
    void    *pBits;
    int     width, height, wid;
    DWORD   clr;
};
//-----------------------------------------------------------------------------------------
class dragonC
{
public:
    dragonC() { bmp.create( BMP_SIZE, BMP_SIZE ); dir = WEST; }
    void draw( int iterations ) { generate( iterations ); draw(); }

private:
    void generate( int it )
    {
	generator.push_back( 1 );
	string temp;

	for( int y = 0; y < it - 1; y++ )
	{
	    temp = generator; temp.push_back( 1 );
	    for( string::reverse_iterator x = generator.rbegin(); x != generator.rend(); x++ )
		temp.push_back( !( *x ) );

	    generator = temp;
	}
    }

    void draw()
    {
	HDC dc = bmp.getDC();
	unsigned int clr[] = { 0xff, 0xff00, 0xff0000, 0x00ffff };
	int mov[] = { 0, 0, 1, -1, 1, -1, 1, 0 }; int i = 0;

	for( int t = 0; t < 4; t++ )
	{
	    int a = BMP_SIZE / 2, b = a; a += mov[i++]; b += mov[i++];
	    MoveToEx( dc, a, b, NULL );

	    bmp.setPenColor( clr[t] );
	    for( string::iterator x = generator.begin(); x < generator.end(); x++ )
	    {
		switch( dir )
		{
		    case NORTH:
			if( *x ) { a += LEN; dir = EAST; }
			else { a -= LEN; dir = WEST; }				
		    break;
		    case EAST:
			if( *x ) { b += LEN; dir = SOUTH; }
			else { b -= LEN; dir = NORTH; }
		    break;
		    case SOUTH:
			if( *x ) { a -= LEN; dir = WEST; }
			else { a += LEN; dir = EAST; }
		    break;
		    case WEST:
			if( *x ) { b -= LEN; dir = NORTH; }
			else { b += LEN; dir = SOUTH; }
		}
	        LineTo( dc, a, b );
	    }
	}
	// !!! change this path !!!
	bmp.saveBitmap( "f:/rc/dragonCpp.bmp" );
    }
	
    int dir;
    myBitmap bmp;
    string generator;
};
//-----------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    dragonC d; d.draw( 17 );
    return system( "pause" );
}
//-----------------------------------------------------------------------------------------
