#include "windows.bi"
#include "win\commctrl.bi"   ' for CreateStatusWindow
Dim Shared As Handle hEdit, hList, hStatus
Function WndProc(hWnd As HWND, msg As  UINT, wParam As WPARAM, lParam As LPARAM) As LRESULT
   Dim As RECT rc
   Dim As PAINTSTRUCT ps
   Dim As HANDLE PtDC
   Dim As HMENU hMenu, hPopup, hPop2
   Select Case msg
   Case WM_CREATE
      hMenu=CreateMenu()               ' create the main menu
      hPopup=CreatePopupMenu()               ' create a sub-menu
      AppendMenu(hMenu, MF_POPUP, Cast(UINT_PTR, hPopup), "&File")      ' add it to the main menu
      AppendMenu(hPopup, MF_STRING, 101, "&Open")       ' one more main item
      hPop2=CreatePopupMenu()               ' create a sub-menu
      AppendMenu(hPop2, MF_STRING, 121, "&sources")      ' fill it
      AppendMenu(hPop2, MF_STRING, 122, "&includes")      ' with various
      AppendMenu(hPop2, MF_STRING, 123, "&DLLs")      ' options
      AppendMenu(hPopup, MF_POPUP, Cast(UINT_PTR, hPop2), "&Dir")      ' and add it to the main menu as "Dir"
      AppendMenu(hPopup, MF_STRING, 102, "&Save")       ' one more main item
      AppendMenu(hPopup, MF_STRING, 103, "E&xit")       ' one more main item
      SetMenu(hWnd, hMenu)               ' attach menu to main window
      hStatus=CreateStatusWindow(WS_CHILD Or WS_VISIBLE Or WS_CLIPSIBLINGS Or SBS_SIZEGRIP, 0, hWnd, 99)
      SendMessage(hStatus, WM_SETTEXT, 0, Cast(lParam, @"This is the status bar"))
      hEdit=CreateWindowEx(WS_EX_CLIENTEDGE, "edit", "Hello, I am an edit control", WS_CHILD Or WS_VISIBLE Or ES_MULTILINE, 0, 0, 100, 100, hWnd, hMenu, 0, 0)
      hList=CreateWindowEx(WS_EX_CLIENTEDGE, "listbox", 0, WS_CHILD Or WS_VISIBLE, 0, 0, 100, 100, hWnd, hMenu, 0, 0)
      SendMessage(hList, LB_ADDSTRING, 0, Cast(lParam, @"line 1"))
      If Open(Command(1) For Binary Access Read As #1) = 0 Then
         Dim As UByte file_char ( LOF(1))
         Get #1, , file_char()
         Close #1
         SendMessage(hEdit, WM_SETTEXT, 0, Cast(lParam, @file_char(0)))
      endif
   Case WM_COMMAND
      Select Case wParam
      Case 101: MessageBox(hWnd, "Open not implemented", 0, MB_OK)
      Case 102: MessageBox(hWnd, "Save not implemented", 0, MB_OK)
      Case 121: MessageBox(hWnd, "No *.bas files found", 0, MB_OK)
      Case 122: MessageBox(hWnd, "No *.inc files found", 0, MB_OK)
      Case 123: MessageBox(hWnd, "No *.dll files found", 0, MB_OK)
      Case 103: SendMessage(hWnd, WM_CLOSE, 0, 0)
      End Select
   Case WM_PAINT
      PtDC=BeginPaint(hWnd, @ps)
      SetBkMode(PtDC, TRANSPARENT)
      TextOut(PtDC, 4, 4, "Greetings from the WM_PAINT handler", 35)
      EndPaint(hWnd, @ps)
   Case WM_KEYDOWN
      if wParam=VK_ESCAPE then SendMessage(hWnd, WM_CLOSE, 0, 0)
   Case WM_SIZE
      GetClientRect(hWnd, @rc)
      MoveWindow(hEdit, 3, 28, rc.right-66, rc.bottom-50, 0)
      MoveWindow(hList, rc.right-60, 28, rc.right-6, rc.bottom-50, 0)
   Case WM_DESTROY
      PostQuitMessage(0)
   End Select
   return DefWindowProc(hwnd, msg, wParam, lParam)
End Function
type pCall as function (xy as any ptr) as long
type DLLVERSIONINFO
   cbSize as long
   dwMajorVersion as long
   dwMinorVersion as long
   dwBuildNumber as long
   dwPlatformID as long
end type
Function WinMain(hInstance As HINSTANCE, hPrevInstance As HINSTANCE, lpCmdLine As LPSTR, nShowCmd As Integer) As Integer
   Dim wc As WNDCLASSEX, msg As MSG, hDll As HANDLE, hIconLib As HANDLE, pGetVersion as pCall, dvi As DLLVERSIONINFO
   dvi.cbSize=sizeof(DLLVERSIONINFO)
   hIconLib=LoadLibrary("shell32")
   wc.hIcon = LoadIcon(hIconLib, Cast(LPCTSTR, 239))   ' get the butterfly icon
   FreeLibrary(hIconLib)
   hDll=LoadLibrary("ComCtl32")
   pGetVersion=Cast(Any Ptr, GetProcAddress(hDll, "DllGetVersion"))
   pGetVersion(@dvi)
   If @dvi.dwMajorVersion Then Print "Using common controls version ";Str(dvi.dwMajorVersion);".";Str(dvi.dwMinorVersion)
   FreeLibrary(hDll)
   wc.cbSize = SizeOf(WNDCLASSEX)
   wc.hbrBackground = GetStockObject(COLOR_BTNFACE+1)
   wc.hCursor = LoadCursor(0, IDC_ARROW)
   wc.hIconSm = wc.hIcon
   wc.hInstance = hInstance
   wc.lpfnWndProc = @WndProc
   wc.lpszClassName = @"FbGui"
   wc.style = CS_HREDRAW Or CS_VREDRAW
   RegisterClassEx(@wc)
   If CreateWindowEx(0, wc.lpszClassName, "Hello World",_
      WS_OVERLAPPEDWINDOW Or WS_VISIBLE, (GetSystemMetrics(SM_CXSCREEN) / 2) - 150,_
      (GetSystemMetrics(SM_CYSCREEN) / 2) - 150, 300, 300, 0, 0, hInstance, 0)=0 Then
      MessageBox(0, "Creating hMain failed miserably", 0, MB_OK)
      Return 0
   End If
   While GetMessage(@msg, 0, 0, 0)
      TranslateMessage(@msg)
      DispatchMessage(@msg)
   Wend
   Return msg.wParam
End Function
WinMain(GetModuleHandle(NULL), NULL, Command(), SW_NORMAL)
