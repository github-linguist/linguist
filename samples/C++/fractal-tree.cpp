#include <windows.h>
#include <string>
#include <math.h>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const float PI = 3.1415926536f;

//--------------------------------------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ) {}
    ~myBitmap()
    {
	DeleteObject( pen );
	DeleteDC( hdc );
	DeleteObject( bmp );
    }

    bool create( int w, int h )
    {
	BITMAPINFO	bi;
	void		*pBits;
	ZeroMemory( &bi, sizeof( bi ) );
	bi.bmiHeader.biSize	   = sizeof( bi.bmiHeader );
	bi.bmiHeader.biBitCount	   = sizeof( DWORD ) * 8;
	bi.bmiHeader.biCompression = BI_RGB;
	bi.bmiHeader.biPlanes	   = 1;
	bi.bmiHeader.biWidth	   =  w;
	bi.bmiHeader.biHeight	   = -h;

	HDC dc = GetDC( GetConsoleWindow() );
	bmp = CreateDIBSection( dc, &bi, DIB_RGB_COLORS, &pBits, NULL, 0 );
	if( !bmp ) return false;

	hdc = CreateCompatibleDC( dc );
	SelectObject( hdc, bmp );
	ReleaseDC( GetConsoleWindow(), dc );

	width = w; height = h;

	return true;
    }

    void setPenColor( DWORD clr )
    {
	if( pen ) DeleteObject( pen );
	pen = CreatePen( PS_SOLID, 1, clr );
	SelectObject( hdc, pen );
    }

    void saveBitmap( string path )
    {
	BITMAPFILEHEADER	fileheader;
	BITMAPINFO			infoheader;
	BITMAP				bitmap;
	DWORD*				dwpBits;
	DWORD				wb;
	HANDLE				file;

	GetObject( bmp, sizeof( bitmap ), &bitmap );

	dwpBits = new DWORD[bitmap.bmWidth * bitmap.bmHeight];
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

	file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL, CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
	WriteFile( file, &fileheader, sizeof( BITMAPFILEHEADER ), &wb, NULL );
	WriteFile( file, &infoheader.bmiHeader, sizeof( infoheader.bmiHeader ), &wb, NULL );
	WriteFile( file, dwpBits, bitmap.bmWidth * bitmap.bmHeight * 4, &wb, NULL );
	CloseHandle( file );

	delete [] dwpBits;
    }

    HDC getDC()     { return hdc; }
    int getWidth()  { return width; }
    int getHeight() { return height; }

private:
    HBITMAP bmp;
    HDC	    hdc;
    HPEN    pen;
    int     width, height;
};
//--------------------------------------------------------------------------------------------------
class vector2
{
public:
    vector2() { x = y = 0; }
    vector2( int a, int b ) { x = a; y = b; }
    void set( int a, int b ) { x = a; y = b; }
    void rotate( float angle_r )
    {
	float _x = static_cast<float>( x ),
	      _y = static_cast<float>( y ),
	       s = sinf( angle_r ),
	       c = cosf( angle_r ),
	       a = _x * c - _y * s,
	       b = _x * s + _y * c;

	x = static_cast<int>( a );
	y = static_cast<int>( b );
    }

    int x, y;
};
//--------------------------------------------------------------------------------------------------
class fractalTree
{
public:
    fractalTree()		      { _ang = DegToRadian( 24.0f ); }
    float DegToRadian( float degree ) { return degree * ( PI / 180.0f ); }

    void create( myBitmap* bmp )
    {
	_bmp = bmp;
	float line_len = 130.0f;

	vector2 sp( _bmp->getWidth() / 2, _bmp->getHeight() - 1 );
	MoveToEx( _bmp->getDC(), sp.x, sp.y, NULL );
	sp.y -= static_cast<int>( line_len );
	LineTo( _bmp->getDC(), sp.x, sp.y);

	drawRL( &sp, line_len, 0, true );
	drawRL( &sp, line_len, 0, false );
    }

private:
    void drawRL( vector2* sp, float line_len, float a, bool rg )
    {
	line_len *= .75f;
	if( line_len < 2.0f ) return;

	MoveToEx( _bmp->getDC(), sp->x, sp->y, NULL );
	vector2 r( 0, static_cast<int>( line_len ) );

        if( rg ) a -= _ang;
        else a += _ang;

	r.rotate( a );
	r.x += sp->x; r.y = sp->y - r.y;

	LineTo( _bmp->getDC(), r.x, r.y );

	drawRL( &r, line_len, a, true );
	drawRL( &r, line_len, a, false );
    }

    myBitmap* _bmp;
    float     _ang;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );

    myBitmap bmp;
    bmp.create( 640, 512 );
    bmp.setPenColor( RGB( 255, 255, 0 ) );

    fractalTree tree;
    tree.create( &bmp );
	
    BitBlt( GetDC( GetConsoleWindow() ), 0, 20, 648, 512, bmp.getDC(), 0, 0, SRCCOPY );

    bmp.saveBitmap( "f://rc//fracTree.bmp" );
	
    system( "pause" );
	
    return 0;
}
//--------------------------------------------------------------------------------------------------
