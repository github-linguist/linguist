#include <windows.h>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const int BMP_SIZE = 600, CELL_SIZE = 4, GRID_SIZE = BMP_SIZE / CELL_SIZE;
const bool INFINIT_RUN = false;

enum cellState { WHITE, BLACK, ANT };
enum facing { NOR, EAS, SOU, WES };
enum state { RUNNING, RESTING };

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
    void   *pBits;
    int	    width, height;
};
//--------------------------------------------------------------------------------------------------
class Ant
{
public:
    Ant()
    {
	_bmp.create( BMP_SIZE, BMP_SIZE );
	ZeroMemory( _grid, sizeof( _grid ) );
	RED_BRUSH = CreateSolidBrush( 255 );
	_antState = RUNNING;
    }

    ~Ant()
    {
	DeleteObject( RED_BRUSH );
    }

    void setPosition( int x, int y )
    {
	_sx = x; _sy = y;
	_facing = WES;
    }

    void mainLoop()
    {
	switch( _antState )
	{
	    case RUNNING:
	        simulate();
		// fall thru
	    case RESTING:
		display();
	}
    }

    void setHWND( HWND hwnd ) { _hwnd = hwnd; }

private:
    void simulate()
    {
	switch( _grid[_sx][_sy] )
	{
	    case BLACK:
		_grid[_sx][_sy] = WHITE;
		if( --_facing < NOR ) _facing = WES;
	    break;
	    case WHITE:
		_grid[_sx][_sy] = BLACK;
		if( ++_facing > WES ) _facing = NOR;
	}
	switch( _facing )
	{
	    case NOR:
		if( --_sy < 0 )
		{
		    if( INFINIT_RUN ) _sy = GRID_SIZE - 1;
		    else _antState = RESTING;
		}
	    break;
	    case EAS:
		if( ++_sx >= GRID_SIZE )
		{
		    if( INFINIT_RUN ) _sx = 0;
		    else _antState = RESTING;
		}
	    break;
	    case SOU:
		if( ++_sy >= GRID_SIZE )
		{
		    if( INFINIT_RUN ) _sy = 0;
		    else _antState = RESTING;
		}
	    break;
	    case WES:
	        if( --_sx < 0 )
		{
		    if( INFINIT_RUN ) _sx = GRID_SIZE - 1;
		    else _antState = RESTING;
		}
	}
    }

    void display()
    {
        _bmp.clear();
		
        HBRUSH br; RECT rc;
        int xx, yy; HDC dc = _bmp.getDC();

        for( int y = 0; y < GRID_SIZE; y++ )
	    for( int x = 0; x < GRID_SIZE; x++ )
	    {
	        switch( _grid[x][y] )
	        {
		    case BLACK: br = static_cast<HBRUSH>( GetStockObject( BLACK_BRUSH ) ); break;
		    case WHITE: br = static_cast<HBRUSH>( GetStockObject( WHITE_BRUSH ) );
	        }
	        if( x == _sx && y == _sy ) br = RED_BRUSH;

	        xx = x * CELL_SIZE; yy = y * CELL_SIZE;
	        SetRect( &rc, xx, yy, xx + CELL_SIZE, yy + CELL_SIZE );
	        FillRect( dc, &rc, br );
	    }

        HDC wdc = GetDC( _hwnd );
        BitBlt( wdc, 0, 0, BMP_SIZE, BMP_SIZE, dc, 0, 0, SRCCOPY );
        ReleaseDC( _hwnd, wdc );
    }

    myBitmap _bmp;
    HWND     _hwnd;
    HBRUSH   RED_BRUSH;
    BYTE     _grid[GRID_SIZE][GRID_SIZE];
    int      _sx, _sy, _facing;
    state    _antState;
};
//--------------------------------------------------------------------------------------------------
class wnd
{
public:
    int wnd::Run( HINSTANCE hInst )
    {
	_hInst = hInst;
	_hwnd = InitAll();

	_ant.setHWND( _hwnd );
	_ant.setPosition( GRID_SIZE / 2, GRID_SIZE / 2 );

	ShowWindow( _hwnd, SW_SHOW );
	UpdateWindow( _hwnd );

	MSG msg;
	ZeroMemory( &msg, sizeof( msg ) );
	while( msg.message != WM_QUIT )
	{
	    if( PeekMessage( &msg, NULL, 0, 0, PM_REMOVE ) != 0 )
	    {
		TranslateMessage( &msg );
		DispatchMessage( &msg );
	    }
	    else
	    {
		_ant.mainLoop();
	    }
	}
	return UnregisterClass( "_LANGTONS_ANT_", _hInst );
    }
private:
    static int WINAPI wnd::WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
    {
	switch( msg )
	{
	    case WM_DESTROY: PostQuitMessage( 0 ); break;
	    default:
		return DefWindowProc( hWnd, msg, wParam, lParam );
	}
	return 0;
    }

    HWND InitAll()
    {
	WNDCLASSEX wcex;
	ZeroMemory( &wcex, sizeof( wcex ) );
	wcex.cbSize	       = sizeof( WNDCLASSEX );
	wcex.style	       = CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc   = ( WNDPROC )WndProc;
	wcex.hInstance     = _hInst;
	wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
	wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
	wcex.lpszClassName = "_LANGTONS_ANT_";

	RegisterClassEx( &wcex );

	return CreateWindow( "_LANGTONS_ANT_", ".: Langton's Ant -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, BMP_SIZE, BMP_SIZE, NULL, NULL, _hInst, NULL );
    }

    HINSTANCE _hInst;
    HWND      _hwnd;
    Ant       _ant;
};
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    wnd myWnd;
    return myWnd.Run( hInstance );
}
//--------------------------------------------------------------------------------------------------
