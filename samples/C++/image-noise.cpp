#include <windows.h>
#include <sstream>
//--------------------------------------------------------------------------------------------------
using namespace std;

//--------------------------------------------------------------------------------------------------
const unsigned int BMP_WID = 320, BMP_HEI = 240, WHITE = 16777215, BLACK = 0;

//--------------------------------------------------------------------------------------------------
class myBitmap
{
public:
    myBitmap() : pen( NULL ), brush( NULL ), clr( 0 ), wid( 1 ) {}
    ~myBitmap()
    {
	DeleteObject( pen ); DeleteObject( brush );
	DeleteDC( hdc ); DeleteObject( bmp );
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
    void setPenWidth( int w ) { wid = w; createPen(); }

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

    void* getBits( void ) const { return pBits; }
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
    void*   pBits;
    int     width, height, wid;
    DWORD   clr;
};
//--------------------------------------------------------------------------------------------------
class bmpNoise
{
public:
    bmpNoise()
    {
	QueryPerformanceFrequency( &_frequency );
	_bmp.create( BMP_WID, BMP_HEI );
	_frameTime = _fps = 0; _start = getTime(); _frames = 0;
    }

    void mainLoop()
    {
	float now = getTime();
	if( now - _start > 1.0f ) { _fps = static_cast<float>( _frames ) / ( now - _start ); _start = now; _frames = 0; }
	HDC wdc, dc = _bmp.getDC();
	unsigned int* bits = reinterpret_cast<unsigned int*>( _bmp.getBits() );

	for( int y = 0; y < BMP_HEI; y++ )
	{
	    for( int x = 0; x < BMP_WID; x++ )
	    {
		if( rand() % 10 < 5 ) memset( bits, 255, 3 );
		else memset( bits, 0, 3 );
		bits++;
	    }
	}
	ostringstream o; o << _fps; TextOut( dc, 0, 0, o.str().c_str(), o.str().size() );

	wdc = GetDC( _hwnd );
	BitBlt( wdc, 0, 0, BMP_WID, BMP_HEI, dc, 0, 0, SRCCOPY );
	ReleaseDC( _hwnd, wdc );
	_frames++; _frameTime = getTime() - now;
	if( _frameTime > 1.0f ) _frameTime = 1.0f;
    }
	
    void setHWND( HWND hwnd ) { _hwnd = hwnd; }

private:
    float getTime()
    {
	LARGE_INTEGER liTime; QueryPerformanceCounter( &liTime );
	return liTime.QuadPart  / ( float )_frequency.QuadPart;
    }
    myBitmap      _bmp;
    HWND          _hwnd;
    float         _start, _fps, _frameTime;
    unsigned int  _frames;
    LARGE_INTEGER _frequency;
};
//--------------------------------------------------------------------------------------------------
class wnd
{
public:
    wnd() { _inst = this; }
    int wnd::Run( HINSTANCE hInst )
    {
	_hInst = hInst; _hwnd = InitAll();
        _noise.setHWND( _hwnd );
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
		_noise.mainLoop();
	    }
	}
	return UnregisterClass( "_MY_NOISE_", _hInst );
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
	wcex.cbSize           = sizeof( WNDCLASSEX );
	wcex.style           = CS_HREDRAW | CS_VREDRAW;
	wcex.lpfnWndProc   = ( WNDPROC )WndProc;
	wcex.hInstance     = _hInst;
	wcex.hCursor       = LoadCursor( NULL, IDC_ARROW );
	wcex.hbrBackground = ( HBRUSH )( COLOR_WINDOW + 1 );
	wcex.lpszClassName = "_MY_NOISE_";

	RegisterClassEx( &wcex );

	RECT rc = { 0, 0, BMP_WID, BMP_HEI };
	AdjustWindowRect( &rc, WS_SYSMENU | WS_CAPTION, FALSE );
	int w = rc.right - rc.left, h = rc.bottom - rc.top;
	return CreateWindow( "_MY_NOISE_", ".: Noise image -- PJorente :.", WS_SYSMENU, CW_USEDEFAULT, 0, w, h, NULL, NULL, _hInst, NULL );
    }

    static wnd* _inst;
    HINSTANCE   _hInst;
    HWND        _hwnd;
    bmpNoise    _noise;
};
wnd* wnd::_inst = 0;
//--------------------------------------------------------------------------------------------------
int APIENTRY _tWinMain( HINSTANCE hInstance, HINSTANCE hPrevInstance, LPTSTR lpCmdLine, int nCmdShow )
{
    srand( GetTickCount() ); wnd myWnd;
    return myWnd.Run( hInstance );
}
//--------------------------------------------------------------------------------------------------
