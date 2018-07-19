#include <windows.h>

//--------------------------------------------------------------------------------------------------
class pinstripe
{
public:
    pinstripe()                        { createColors(); }
    void setDimensions( int x, int y ) { _mw = x; _mh = y; }
    void createColors()
    {
	colors[0] = 0; colors[1] = 255; colors[2] = RGB( 0, 255, 0 );
	colors[3] = RGB( 0, 0, 255 ); colors[4] = RGB( 255, 0, 255 );
	colors[5] = RGB( 0, 255, 255 ); colors[6] = RGB( 255, 255, 0 );
	colors[7] = RGB( 255, 255, 255 );
    }

    void draw( HDC dc )
    {
        HPEN pen;
	int lh = _mh / 4, row, cp;
	for( int lw = 1; lw < 5; lw++ )
	{
	    cp = 0;
            row = ( lw - 1 ) * lh;
	    for( int x = 0 + lw > 1 ? lw > 3 ? 2 : 1 : 0; x < _mw; x += lw )
	    {
		pen = CreatePen( PS_SOLID, lw, colors[cp] );
	        ++cp %= 8;

		SelectObject( dc, pen );
		MoveToEx( dc, x, row, NULL );
		LineTo( dc, x, row + lh );
		DeleteObject( pen );
	    }
	}
    }

private:
    int _mw, _mh;
    DWORD colors[8];
};
//--------------------------------------------------------------------------------------------------
pinstripe pin;

//--------------------------------------------------------------------------------------------------
void PaintWnd( HWND hWnd )
{
    PAINTSTRUCT ps;
    HDC hdc = BeginPaint( hWnd, &ps );
    pin.draw( hdc );
    EndPaint( hWnd, &ps );
}
//--------------------------------------------------------------------------------------------------
LRESULT CALLBACK WndProc( HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam )
{
    switch( msg )
    {
	case WM_DESTROY: PostQuitMessage( 0 ); break;
	case WM_PAINT: PaintWnd( hWnd ); break;
	default:
	    return DefWindowProc( hWnd, msg, wParam, lParam );
    }
    return 0;
}
//--------------------------------------------------------------------------------------------------
HWND InitAll( HINSTANCE hInstance )
{
    WNDCLASSEX wcex;
    ZeroMemory( &wcex, sizeof( wcex ) );

    wcex.cbSize	       = sizeof( WNDCLASSEX );
    wcex.style	       = CS_HREDRAW | CS_VREDRAW;
    wcex.lpfnWndProc   = WndProc;
    wcex.hInstance     = hInstance;
    wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
    wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
    wcex.lpszClassName = "_CLR_PS_";

    RegisterClassEx( &wcex );
    return CreateWindow( "_CLR_PS_", ".: Clr Pinstripe -- PJorente :.", WS_POPUP, CW_USEDEFAULT, 0, 200, 200, NULL, NULL, hInstance, NULL );
}
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    srand( GetTickCount() );

    HWND hwnd = InitAll( hInstance );
    if( !hwnd ) return -1;

    int mw = GetSystemMetrics( SM_CXSCREEN ),
	mh = GetSystemMetrics( SM_CYSCREEN );

    pin.setDimensions( mw, mh );

    RECT rc = { 0, 0, mw, mh };

    AdjustWindowRectEx( &rc, WS_POPUP, FALSE, 0 );
    int w = rc.right  - rc.left,
	h = rc.bottom - rc.top;

    int posX = ( GetSystemMetrics( SM_CXSCREEN ) >> 1 ) - ( w >> 1 ),
	posY = ( GetSystemMetrics( SM_CYSCREEN ) >> 1 ) - ( h >> 1 );

    SetWindowPos( hwnd, HWND_TOP, posX, posY, w, h, SWP_NOZORDER );
    ShowWindow( hwnd, nCmdShow );
    UpdateWindow( hwnd );

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
    return UnregisterClass( "_CLR_PS_", hInstance );
}
//--------------------------------------------------------------------------------------------------
