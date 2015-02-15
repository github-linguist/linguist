#include <windows.h>
#include <iostream>
#include <string>

//--------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------
enum states { SEED, GROWING, MOVING, REST };
enum treeStates { NONE, MOVER, TREE };
const int MAX_SIDE = 480, MAX_MOVERS = 511, MAX_CELLS = 15137;

//--------------------------------------------------------------------
class point
{
public:
    point()                  { x = y = 0; }
    point( int a, int b )    { x = a; y = b; }
    void set( int a, int b ) { x = a; y = b; }

    int x, y;
};
//--------------------------------------------------------------------
class movers
{
public:
    point pos;
    bool moving;
    movers() : moving( false ){}
};
//--------------------------------------------------------------------
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

	bi.bmiHeader.biSize        = sizeof( bi.bmiHeader );
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

	file = CreateFile( path.c_str(), GENERIC_WRITE, 0, NULL,
                           CREATE_ALWAYS, FILE_ATTRIBUTE_NORMAL, NULL );
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
    void    *pBits;
    int	    width, height;
};
//--------------------------------------------------------------------
class brownianTree
{
public:
    brownianTree()
    {
	_bmp.create( MAX_SIDE, MAX_SIDE );
	init();
    }

    void init()
    {
	_cellCount = 0;
	ZeroMemory( _grid, sizeof( _grid ) );
	_bmp.clear();
	_state = SEED;
    }

bool mainLoop()
    {
	switch( _state )
	{
	    case REST:    saveTree(); return false;
	    case SEED:    doSeed(); break;
	    case GROWING: startMovers(); break;
	    case MOVING:  moveMovers();
	}
	    return true;
	}

    myBitmap* getBmp() { return &_bmp; }

private:
    void saveTree()
    {
	for( int y = 0; y < MAX_SIDE; y++ )
	    for( int x = 0; x < MAX_SIDE; x++ )
		if( _grid[x][y] == TREE )
		    SetPixel( _bmp.getDC(), x, y, RGB( 255, 120, 0 ) );

        _bmp.saveBitmap( "f:\\rc\\tree.bmp" );
    }

    void doSeed()
    {
	int x = MAX_SIDE - MAX_SIDE / 2, y = MAX_SIDE / 4;
	_grid[rand() % x + y][rand() % x + y] = TREE;
	_cellCount++;
	_state = GROWING;
    }

    void addMover( movers* m )
    {
	m->moving = true;
	int x = MAX_SIDE - MAX_SIDE / 2, y = MAX_SIDE / 4, a, b;
	while( true )
	{
	    a = rand() % x + y; b = rand() % x + y;
	    if( _grid[a][b] == NONE ) break;
	}

	m->pos.set( a, b );
	_grid[a][b] = MOVER;
    }

    void startMovers()
    {
	movers* m;
	for( int c = 0; c < MAX_MOVERS; c++ )
	{
	    m = &_movers[c];
	    addMover( m );
	}
	_state = MOVING;
    }

    void addToTree( movers* m )
    {
	m->moving = false;
	_grid[m->pos.x][m->pos.y] = TREE;
	if( ++_cellCount >= MAX_CELLS ) _state = REST;

	COORD c = { 0, 1 };
	SetConsoleCursorPosition( GetStdHandle( STD_OUTPUT_HANDLE ), c );
	cout << "Cells added: " << _cellCount
             << " from " << MAX_CELLS << " => "
             <<  static_cast<float>( 100 * _cellCount ) /
                 static_cast<float>( MAX_CELLS )
             << "%              ";
    }

    bool moveIt( movers* m )
    {
	point f[8]; int ff = 0;
	for( int y = -1; y < 2; y++ )
	{
	    for( int x = -1; x < 2; x++ )
	    {
		if( !x && !y ) continue;
		int a = m->pos.x + x, b = m->pos.y + y;
		if( a < 0 || b < 0 || a >= MAX_SIDE || b >= MAX_SIDE )
		{
		    addToTree( m );
		    return true;
		}
		switch( _grid[a][b] )
		{
		    case TREE:
			addToTree( m );
			return true;
		    case NONE:
			f[ff++].set( a, b );
		}
	    }
        }

	if( ff < 1 ) return false;

	_grid[m->pos.x][m->pos.y] = NONE;
	m->pos = f[rand() % ff];
	_grid[m->pos.x][m->pos.y] = MOVER;

	return false;
    }

    void moveMovers()
    {
	movers* mm;
	for( int m = 0; m < MAX_MOVERS; m++ )
	{
	    mm = &_movers[m];
	    if( !mm->moving ) continue;
	    if( moveIt( mm ) && _cellCount < MAX_CELLS ) addMover( mm );
	}
    }

    states   _state;
    BYTE     _grid[MAX_SIDE][MAX_SIDE];
    myBitmap _bmp;
    int      _cellCount;
    movers   _movers[MAX_MOVERS];
};
//--------------------------------------------------------------------
int main( int argc, char* argv[] )
{
    ShowWindow( GetConsoleWindow(), SW_MAXIMIZE );
    srand( GetTickCount() );

    brownianTree tree;

    DWORD now = GetTickCount();
    while( tree.mainLoop() );

    now = GetTickCount() - now;
    cout << endl << endl << "It took "
         << now / 1000
         << " seconds to complete the task!" << endl << endl;

    BitBlt( GetDC( GetConsoleWindow() ), 20, 90, MAX_SIDE, MAX_SIDE,
            tree.getBmp()->getDC(), 0, 0, SRCCOPY );

    system( "pause" );
    return 0;
}
//--------------------------------------------------------------------
