#include <windows.h>
#include <string>
#include <vector>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int HSTEP = 46, MWID = 40, MHEI = 471;
const float VSTEP = 2.3f;

//--------------------------------------------------------------------------------------------------
class vector2
{
public:
    vector2() { x = y = 0; }
    vector2( float a, float b )  { x = a; y = b; }
    void set( float a, float b ) { x = a; y = b; }
    float x, y;
};
//--------------------------------------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ), brush( NULL ), clr( 0 ), wid( 1 ) {}
    ~myBitmap()
    {
	DeleteObject( pen );
	DeleteObject( brush );
	DeleteDC( hdc );
	DeleteObject( bmp );
    }

    bool create( int w, int h )
    {
	BITMAPINFO    bi;
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

    void setPenColor( DWORD c ) { clr = c; createPen(); }

    void setPenWidth( int w )   { wid = w; createPen(); }

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
//--------------------------------------------------------------------------------------------------
class plot
{
public:
    plot() { bmp.create( 512, 512 ); }

    void draw( vector<vector2>* pairs )
    {
	bmp.clear( 0xff );
	drawGraph( pairs );
	plotIt( pairs );

	HDC dc = GetDC( GetConsoleWindow() );
	BitBlt( dc, 0, 30, 512, 512, bmp.getDC(), 0, 0, SRCCOPY );
	ReleaseDC( GetConsoleWindow(), dc );
	//bmp.saveBitmap( "f:\\rc\\plot.bmp" );
    }

private:
    void drawGraph( vector<vector2>* pairs )
    {
	HDC dc = bmp.getDC();
	bmp.setPenColor( RGB( 240, 240, 240 ) );
	DWORD b = 11, c = 40, x;
	RECT rc; char txt[8];

	for( x = 0; x < pairs->size(); x++ )
	{
	    MoveToEx( dc, 40, b, NULL ); LineTo( dc, 500, b );
	    MoveToEx( dc, c, 11, NULL ); LineTo( dc, c, 471 );

	    wsprintf( txt, "%d", ( pairs->size() - x ) * 20 );
	    SetRect( &rc, 0, b - 9, 36, b + 11 );
	    DrawText( dc, txt, lstrlen( txt ), &rc, DT_RIGHT | DT_VCENTER | DT_SINGLELINE );

	    wsprintf( txt, "%d", x );
	    SetRect( &rc, c - 8, 472, c + 8, 492 );
	    DrawText( dc, txt, lstrlen( txt ), &rc, DT_CENTER | DT_VCENTER | DT_SINGLELINE );

	    c += 46; b += 46;
	}

	SetRect( &rc, 0, b - 9, 36, b + 11 );
	DrawText( dc, "0", 1, &rc, DT_RIGHT | DT_VCENTER | DT_SINGLELINE );

	bmp.setPenColor( 0 ); bmp.setPenWidth( 3 );
	MoveToEx( dc, 40, 11, NULL ); LineTo( dc, 40, 471 );
	MoveToEx( dc, 40, 471, NULL ); LineTo( dc, 500, 471 );
    }

    void plotIt( vector<vector2>* pairs )
    {	
	HDC dc = bmp.getDC();
	HBRUSH br = CreateSolidBrush( 255 );
	RECT rc;
		
	bmp.setPenColor( 255 ); bmp.setPenWidth( 2 );
	vector<vector2>::iterator it = pairs->begin();
	int a = MWID + HSTEP * static_cast<int>( ( *it ).x ), b = MHEI - static_cast<int>( VSTEP * ( *it ).y );
	MoveToEx( dc, a, b, NULL );
	SetRect( &rc, a - 3, b - 3, a + 3, b + 3 ); FillRect( dc, &rc, br );

	it++;
	for( ; it < pairs->end(); it++ )
	{
	    a = MWID + HSTEP * static_cast<int>( ( *it ).x );
	    b = MHEI - static_cast<int>( VSTEP * ( *it ).y );
	    SetRect( &rc, a - 3, b - 3, a + 3, b + 3 );
	    FillRect( dc, &rc, br ); LineTo( dc, a, b );
	}

	DeleteObject( br );
    }

    myBitmap bmp;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );
    plot pt;
    vector<vector2> pairs;
    pairs.push_back( vector2( 0, 2.7f ) ); pairs.push_back( vector2( 1, 2.8f ) );
    pairs.push_back( vector2( 2.0f, 31.4f ) ); pairs.push_back( vector2( 3.0f, 38.1f ) );
    pairs.push_back( vector2( 4.0f, 58.0f ) ); pairs.push_back( vector2( 5.0f, 76.2f ) );
    pairs.push_back( vector2( 6.0f, 100.5f ) ); pairs.push_back( vector2( 7.0f, 130.0f ) );
    pairs.push_back( vector2( 8.0f, 149.3f ) ); pairs.push_back( vector2( 9.0f, 180.0f ) );
	
    pt.draw( &pairs );
    system( "pause" );

    return 0;
}
//--------------------------------------------------------------------------------------------------
