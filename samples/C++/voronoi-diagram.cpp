#include <windows.h>
#include <vector>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
class point
{
public:
    int x, y;

    point() { x = y = 0; }
    point( int a, int b ) { x = a; y = b; }
    int distanceSqrd( const point& p )
    {
	int xd = p.x - x,
	    yd = p.y - y;

	return xd * xd + yd * yd;
    }
};
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
	BITMAPFILEHEADER fileheader;
	BITMAPINFO	 infoheader;
	BITMAP		 bitmap;
	DWORD*		 dwpBits;
	DWORD		 wb;
	HANDLE		 file;

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
    int	    width, height;
};
//--------------------------------------------------------------------------------------------------
class Voronoi
{
public:
    void make( myBitmap* bmp, int count )
    {
	_bmp = bmp;
	createPoints( count );
	createColors();
	createSites();
	setSitesPoints();
    }

private:
    void createSites()
    {
	int w = _bmp->getWidth(), h = _bmp->getHeight(), d;
	for( int hh = 0; hh < h; hh++ )
	{
	    for( int ww = 0; ww < w; ww++ )
	    {
		point bpt( ww, hh );
		int ind = -1, dist = INT_MAX;
		for( int it = 0; it < points.size(); it++ )
		{
		    d = ( points[it] ).distanceSqrd( bpt );
		    if( d < dist )
		    {
			dist = d;
			ind = it;
		    }
		}

		if( ind > -1 )
		    SetPixel( _bmp->getDC(), ww, hh, colors[ind] );
		else
		    __asm nop // should never happen!
	    }
        }
    }

    void setSitesPoints()
    {
	for( vector<point>::iterator it = points.begin(); it < points.end(); it++ )
	{
	    int x = ( *it ).x, y = ( *it ).y;
	    for( int i = -1; i < 2; i++ )
		for( int j = -1; j < 2; j++ )
		    SetPixel( _bmp->getDC(), x + i, y + j, 0 );
	}
    }

    void createPoints( int count )
    {
	int w = _bmp->getWidth() - 20, h = _bmp->getHeight() - 20;
	for( int i = 0; i < count; i++ )
	{
	    point p( rand() % w + 10, rand() % h + 10 );
	    points.push_back( p );
	}
    }

    void createColors()
    {
	for( int i = 0; i < points.size(); i++ )
	{
	    DWORD c = RGB( rand() % 200 + 50, rand() % 200 + 55, rand() % 200 + 50 );
	    colors.push_back( c );
	}
    }

    vector<point> points;
    vector<DWORD> colors;
    myBitmap* _bmp;
};
//--------------------------------------------------------------------------------------------------
int main(int argc, char* argv[])
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );
    srand( GetTickCount() );

    myBitmap bmp;
    bmp.create( 512, 512 );
    bmp.setPenColor( 0 );
	
    Voronoi v;
    v.make( &bmp, 50 );

    BitBlt( GetDC( GetConsoleWindow() ), 20, 20, 512, 512, bmp.getDC(), 0, 0, SRCCOPY );
    bmp.saveBitmap( "f://rc//v.bmp" );

    system( "pause" );

    return 0;
}
//--------------------------------------------------------------------------------------------------
