#include <windows.h>
#include <math.h>

//--------------------------------------------------------------------------------------------------
const int BMP_SIZE = 738;

//--------------------------------------------------------------------------------------------------
class Sierpinski
{
public:
    void draw( HDC wdc, int wid, int hei, int ord )
    {
	_wdc = wdc;
        _ord = wid / static_cast<int>( pow( 3.0, ord ) );
	drawIt( 0, 0, wid, hei );
    }

    void setHWND( HWND hwnd ) { _hwnd = hwnd; }

private:
    void drawIt( int x, int y, int wid, int hei )
    {
	if( wid < _ord || hei < _ord ) return;
	int w = wid / 3, h = hei / 3;
	RECT rc;
	SetRect( &rc, x + w, y + h, x + w + w, y + h + h );
	FillRect( _wdc, &rc, static_cast<HBRUSH>( GetStockObject( BLACK_BRUSH ) ) );
		
	for( int a = 0; a < 3; a++ )
	    for( int b = 0; b < 3; b++ )
	    {
		if( a == 1 && b == 1 ) continue;
		drawIt( x + b * w, y + a * h, w, h );
	    }
    }

    HWND     _hwnd;
    HDC      _wdc;
    int      _ord;
};
//--------------------------------------------------------------------------------------------------
class wnd
{
public:
    wnd() { _inst = this; }
    int wnd::Run( HINSTANCE hInst )
    {
	_hInst = hInst;
	_hwnd = InitAll();

	_carpet.setHWND( _hwnd );

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
	}
	return UnregisterClass( "_SIERPINSKI_", _hInst );
    }
private:
    void wnd::doPaint( HDC dc ) { _carpet.draw( dc, BMP_SIZE, BMP_SIZE, 5 ); }

    static int WINAPI wnd::WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
    {
	switch( msg )
	{
	    case WM_DESTROY: PostQuitMessage( 0 ); break;
	    case WM_PAINT:
	    {
		PAINTSTRUCT ps;
		HDC dc = BeginPaint( hWnd, &ps );
		_inst->doPaint( dc );
		EndPaint( hWnd, &ps );
	    }		
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
	wcex.lpszClassName = "_SIERPINSKI_";

	RegisterClassEx( &wcex );

	RECT rc = { 0, 0, BMP_SIZE, BMP_SIZE };
	AdjustWindowRect( &rc, WS_SYSMENU | WS_CAPTION, FALSE );
	int w = rc.right - rc.left,
	    h = rc.bottom - rc.top;
	return CreateWindow( "_SIERPINSKI_", ".: Sierpinski carpet -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, w, h, NULL, NULL, _hInst, NULL );
    }

    static wnd* _inst;
    HINSTANCE  _hInst;
    HWND       _hwnd;
    Sierpinski _carpet;
};
wnd* wnd::_inst = 0;
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    wnd myWnd;
    return myWnd.Run( hInstance );
}
//--------------------------------------------------------------------------------------------------
