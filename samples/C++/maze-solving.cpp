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

    BYTE* getMaze() const { return _world; }

    void create( int side )
    {
	_s = side;
	generate();
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
class mazeSolver
{
public:
    mazeSolver()
    {
	_bmp.create( BMP_SIZE, BMP_SIZE );
	_pts = 0;
    }

    ~mazeSolver() { killPoints(); }

    void solveIt( BYTE* maze, int size, int sX, int sY, int eX, int eY )
    {
	_lastDir = NONE;
	_world = maze; _s = size; _sx = sX; _sy = sY; _ex = eX; _ey = eY;
		
	for( int y = 0; y < _s; y++ )
	    for( int x = 0; x < _s; x++ )
		_world[x + _s * y] &= 0x0f;

        _world[_sx + _s * _sy] |= NOR << 4;

	killPoints();
	_pts = new BYTE[_s * _s];
	ZeroMemory( _pts, _s * _s );
		
	findTheWay();

	_sx = sX; _sy = sY;
	display();
    }

private:
    int invert( int d )
    {
	switch( d )
	{
	    case NOR: return SOU;
	    case SOU: return NOR;
	    case WES: return EAS;
	    case EAS: return WES;
	}
	return NONE;
    }

    void updatePosition( int d )
    {
        switch( d )
	{
	    case NOR: _sy--; break;
	    case EAS: _sx++; break;
	    case SOU: _sy++; break;
	    case WES: _sx--;
	}
    }

    void findTheWay()
    {
	while( true )
	{
	    int d = getDirection();
	    if( d < NOR ) return;
	    _lastDir = invert( d );
	    _world[_sx + _s * _sy] |= d;
	    _pts[_sx + _s * _sy] = d;
	    updatePosition( d );
	    if( _sx == _ex && _sy == _ey ) return;
	    _world[_sx + _s * _sy] |= _lastDir << 4;
	}
    }

    int getDirection()
    {
	int d = 1 << rand() % 4;
	while( true )
	{
	    for( int x = 0; x < 4; x++ )
	    {
		if( testDirection( d ) ) return d;
		d <<= 1;
		if( d > 8 ) d = 1;
	    }

	    d = ( _world[_sx + _s * _sy] & 0xf0 ) >> 4;
	    if( !d ) return -1;
	    _pts[_sx + _s * _sy] = 0;
	    updatePosition( d );
	    _lastDir = invert( d );
	    d = 1 << rand() % 4;
	}
    }

    bool testDirection( int d )
    {
	if( d == _lastDir || !( _world[_sx + _s * _sy] & d ) ) return false;
	switch( d )
	{
	    case NOR:
		return _sy - 1 > -1 && !( _world[_sx + _s * ( _sy - 1 )] & 0xf0 );
	    case EAS:
		return _sx + 1 < _s && !( _world[_sx + 1 + _s * _sy] & 0xf0 );
	    case SOU:
		return _sy + 1 < _s && !( _world[_sx + _s * ( _sy + 1 )] & 0xf0 );
	    case WES:
		return _sx - 1 > -1 && !( _world[_sx - 1 + _s * _sy] & 0xf0 );
	}
	return false;
    }

    void display()
    {
	_bmp.setPenColor( RGB( 0, 255, 0 ) );
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

	drawEndPoints( dc );
	_bmp.setPenColor( RGB( 255, 0, 0 ) );

	for( int y = 0; y < _s; y++ )
	{
	    int yy = y * _s;
	    for( int x = 0; x < _s; x++ )
	    {
		BYTE d = _pts[x + yy];
		if( !d ) continue;

		int nx = x * CELL_SIZE + 4,
		    ny = y * CELL_SIZE + 4;

		MoveToEx( dc, nx, ny, NULL );
		switch( d )
		{
		    case NOR: LineTo( dc, nx, ny - CELL_SIZE - 1 ); break;
		    case EAS: LineTo( dc, nx + CELL_SIZE + 1, ny ); break;
		    case SOU: LineTo( dc, nx, ny + CELL_SIZE + 1 ); break;
		    case WES: LineTo( dc, nx - CELL_SIZE - 1, ny ); break;
		}
	    }
	}

	_bmp.saveBitmap( "f:\\rc\\maze_s.bmp" );
	BitBlt( GetDC( GetConsoleWindow() ), 10, 60, BMP_SIZE, BMP_SIZE, _bmp.getDC(), 0, 0, SRCCOPY );
    }

    void drawEndPoints( HDC dc )
    {
	RECT rc;
	int x = 1 + _sx * CELL_SIZE, y = 1 + _sy * CELL_SIZE;
	SetRect( &rc, x, y, x + CELL_SIZE - 1, y + CELL_SIZE - 1 );
	FillRect( dc, &rc, ( HBRUSH )GetStockObject( WHITE_BRUSH ) );
	x = 1 + _ex * CELL_SIZE, y = 1 + _ey * CELL_SIZE;
	SetRect( &rc, x, y, x + CELL_SIZE - 1, y + CELL_SIZE - 1 );
	FillRect( dc, &rc, ( HBRUSH )GetStockObject( WHITE_BRUSH ) );
    }

    void killPoints() { if( _pts ) delete [] _pts; }

    BYTE*    _world, *_pts;
    int      _s, _sx, _sy, _ex, _ey, _lastDir;
    myBitmap _bmp;
};
//--------------------------------------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );
    srand( GetTickCount() );

    mazeGenerator mg;
    mazeSolver ms;
    int s;
    while( true )
    {
	cout << "Enter the maze size, an odd number bigger than 2 ( 0 to QUIT ): "; cin >> s;
	if( !s ) return 0;
	if( !( s & 1 ) ) s++;
	if( s >= 3 )
	{
	    mg.create( s );
	    int sx, sy, ex, ey;
	    while( true )
	    {
		sx = rand() % s; sy = rand() % s;
		ex = rand() % s; ey = rand() % s;
		if( ex != sx || ey != sy ) break;
	    }
	    ms.solveIt( mg.getMaze(), s, sx, sy, ex, ey );
	    cout << endl;
	}
	system( "pause" );
	system( "cls" );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------
