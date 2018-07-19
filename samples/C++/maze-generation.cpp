#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int BMP_SIZE = 512, CELL_SIZE = 8;

//--------------------------------------------------------------------------------------------------
enum directions { NONE, NOR = 1, EAS = 2, SOU = 4, WES = 8 };

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

    void clear()
    {
	ZeroMemory( pBits, width * height * sizeof( DWORD ) );
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
	DWORD		 wb;

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
    HBITMAP bmp;
    HDC	    hdc;
    HPEN    pen;
    void    *pBits;
    int	    width, height;
};
//--------------------------------------------------------------------------------------------------
class mazeGenerator
{
public:
    mazeGenerator()
    {
	_world = 0;
	_bmp.create( BMP_SIZE, BMP_SIZE );
	_bmp.setPenColor( RGB( 0, 255, 0 ) );
    }

    ~mazeGenerator() { killArray(); }

    void create( int side )
    {
	_s = side;
	generate();
	display();
    }

private:
    void generate()
    {
	killArray();
	_world = new BYTE[_s * _s];
	ZeroMemory( _world, _s * _s );
	_ptX = rand() % _s; _ptY = rand() % _s;
	carve();
    }

    void carve()
    {
	while( true )
	{
	    int d = getDirection();
	    if( d < NOR ) return;

	    switch( d )
	    {
	        case NOR:
	            _world[_ptX + _s * _ptY] |= NOR; _ptY--;
		    _world[_ptX + _s * _ptY] = SOU | SOU << 4;
		break;
	        case EAS:
		    _world[_ptX + _s * _ptY] |= EAS; _ptX++;
		    _world[_ptX + _s * _ptY] = WES | WES << 4;
		break;
		case SOU:
		    _world[_ptX + _s * _ptY] |= SOU; _ptY++;
		    _world[_ptX + _s * _ptY] = NOR | NOR << 4;
		break;
		case WES:
		    _world[_ptX + _s * _ptY] |= WES; _ptX--;
		    _world[_ptX + _s * _ptY] = EAS | EAS << 4;
	    }
	}
    }

    void display()
    {
	_bmp.clear();
	HDC dc = _bmp.getDC();
	for( int y = 0; y < _s; y++ )
	{
	    int yy = y * _s;
	    for( int x = 0; x < _s; x++ )
	    {
		BYTE b = _world[x + yy];
		int nx = x * CELL_SIZE,
		    ny = y * CELL_SIZE;
				
		if( !( b & NOR ) )
		{
		    MoveToEx( dc, nx, ny, NULL );
		    LineTo( dc, nx + CELL_SIZE + 1, ny );
		}
		if( !( b & EAS ) )
		{
		    MoveToEx( dc, nx + CELL_SIZE, ny, NULL );
		    LineTo( dc, nx + CELL_SIZE, ny + CELL_SIZE + 1 );
		}
		if( !( b & SOU ) )
		{
		    MoveToEx( dc, nx, ny + CELL_SIZE, NULL );
		    LineTo( dc, nx + CELL_SIZE + 1, ny + CELL_SIZE );
		}
		if( !( b & WES ) )
		{
		    MoveToEx( dc, nx, ny, NULL );
		    LineTo( dc, nx, ny + CELL_SIZE + 1 );
		}
	    }
	}

	//_bmp.saveBitmap( "f:\\rc\\maze.bmp" );
	BitBlt( GetDC( GetConsoleWindow() ), 10, 60, BMP_SIZE, BMP_SIZE, _bmp.getDC(), 0, 0, SRCCOPY );
    }

    int getDirection()
    {
	int d = 1 << rand() % 4;
	while( true )
	{
	    for( int x = 0; x < 4; x++ )
	    {
		if( testDir( d ) ) return d;
		d <<= 1;
		if( d > 8 ) d = 1;
	    }
	    d = ( _world[_ptX + _s * _ptY] & 0xf0 ) >> 4;
	    if( !d ) return -1;
	    switch( d )
	    {
		case NOR: _ptY--; break;
		case EAS: _ptX++; break;
		case SOU: _ptY++; break;
		case WES: _ptX--; break;
	    }
            d = 1 << rand() % 4;
	}
    }

    bool testDir( int d )
    {
	switch( d )
	{
	    case NOR: return ( _ptY - 1 > -1 && !_world[_ptX + _s * ( _ptY - 1 )] );
	    case EAS: return ( _ptX + 1 < _s && !_world[_ptX + 1 + _s * _ptY] );
	    case SOU: return ( _ptY + 1 < _s && !_world[_ptX + _s * ( _ptY + 1 )] );
	    case WES: return ( _ptX - 1 > -1 && !_world[_ptX - 1 + _s * _ptY] );
	}
	return false;
    }

    void killArray() { if( _world ) delete [] _world; }

    BYTE*    _world;
    int      _s, _ptX, _ptY;
    myBitmap _bmp;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );
    srand( GetTickCount() );

    mazeGenerator mg;
    int s;
    while( true )
    {
	cout << "Enter the maze size, an odd number bigger than 2 ( 0 to QUIT ): "; cin >> s;
	if( !s ) return 0;
	if( !( s & 1 ) ) s++;
	if( s >= 3 ) mg.create( s );
	cout << endl;
	system( "pause" );
	system( "cls" );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------
