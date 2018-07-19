#include <windows.h>
#include <string>

//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
enum states { NONE, TREE, FIRE };
const int MAX_SIDE = 500;

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
    void	*pBits;
    int	    width, height;
};
//--------------------------------------------------------------------------------------------------
class forest
{
public:
    forest()
    {
	_bmp.create( MAX_SIDE, MAX_SIDE );
	initForest( 0.05f, 0.005f );
    }

    void initForest( float p, float f )
    {
	_p = p; _f = f;
	seedForest();
    }

    void mainLoop()
    {
	display();
	simulate();
    }

    void setHWND( HWND hwnd ) { _hwnd = hwnd; }

private:
    float probRand() { return ( float )rand() / 32768.0f; }
	
    void display()
    {
	HDC bdc = _bmp.getDC();
	DWORD clr;

	for( int y = 0; y < MAX_SIDE; y++ )
	{
	    for( int x = 0; x < MAX_SIDE; x++ )
	    {
		switch( _forest[x][y] )
		{
		    case FIRE: clr = 255; break;
		    case TREE: clr = RGB( 0, 255, 0 ); break;
		    default: clr = 0;
		}

		SetPixel( bdc, x, y, clr );
	    }
	}

	HDC dc = GetDC( _hwnd );
	BitBlt( dc, 0, 0, MAX_SIDE, MAX_SIDE, _bmp.getDC(), 0, 0, SRCCOPY );
	ReleaseDC( _hwnd, dc );
    }

    void seedForest()
    {
	ZeroMemory( _forestT, sizeof( _forestT ) );
	ZeroMemory( _forest, sizeof( _forest ) );
	for( int y = 0; y < MAX_SIDE; y++ )
	    for( int x = 0; x < MAX_SIDE; x++ )
		if( probRand() < _p ) _forest[x][y] = TREE;
    }

    bool getNeighbors( int x, int y )
    {
	int a, b;
	for( int yy = -1; yy < 2; yy++ )
	    for( int xx = -1; xx < 2; xx++ )
	    {
		if( !xx && !yy ) continue;
		a = x + xx; b = y + yy;
		if( a < MAX_SIDE && b < MAX_SIDE && a > -1 && b > -1 )
		if( _forest[a][b] == FIRE ) return true;
	    }

	return false;
    }

    void simulate()
    {
	for( int y = 0; y < MAX_SIDE; y++ )
	{
	    for( int x = 0; x < MAX_SIDE; x++ )
	    {
		switch( _forest[x][y] )
		{
		    case FIRE: _forestT[x][y] = NONE; break;
		    case NONE: if( probRand() < _p ) _forestT[x][y] = TREE; break;
		    case TREE: if( getNeighbors( x, y ) || probRand() < _f ) _forestT[x][y] = FIRE;
		}
	    }
	}

	for( int y = 0; y < MAX_SIDE; y++ )
	    for( int x = 0; x < MAX_SIDE; x++ )
		_forest[x][y] = _forestT[x][y];
    }

    myBitmap _bmp;
    HWND     _hwnd;
    BYTE     _forest[MAX_SIDE][MAX_SIDE], _forestT[MAX_SIDE][MAX_SIDE];
    float    _p, _f;
};
//--------------------------------------------------------------------------------------------------
class wnd
{
public:
    int wnd::Run( HINSTANCE hInst )
    {
	_hInst = hInst;
	_hwnd = InitAll();

	_ff.setHWND( _hwnd );
	_ff.initForest( 0.02f, 0.001f );

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
		_ff.mainLoop();
	    }
	}
	return UnregisterClass( "_FOREST_FIRE_", _hInst );
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
	wcex.lpszClassName = "_FOREST_FIRE_";

	RegisterClassEx( &wcex );

	return CreateWindow( "_FOREST_FIRE_", ".: Forest Fire -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, MAX_SIDE, MAX_SIDE, NULL, NULL, _hInst, NULL );
    }

    HINSTANCE _hInst;
    HWND      _hwnd;
    forest    _ff;
};
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    srand( GetTickCount() );
    wnd myWnd;
    return myWnd.Run( hInstance );
}
//--------------------------------------------------------------------------------------------------
