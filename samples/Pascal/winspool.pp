unit winspool;

interface

{$PACKRECORDS C}

{$ifndef NO_SMART_LINK}
{$smartlink on}
{$endif}

  uses
    ctypes,windows;
{
  Automatically converted by H2Pas 1.0.0 from winspool.h
  The following command line parameters were used:
    winspool.h
    -D
    -w
}

  const
    External_library='winspool.drv';

  type
    PPVOID = ppointer;

  const
     PORT_STATUS_WARMING_UP = 11;
     PORT_STATUS_POWER_SAVE = 12;

  type
     _PROVIDOR_INFO_1A = record
          pName : LPSTR;
          pEnvironment : LPSTR;
          pDLLName : LPSTR;
       end;
     PROVIDOR_INFO_1A = _PROVIDOR_INFO_1A;
     PPROVIDOR_INFO_1A = ^_PROVIDOR_INFO_1A;
     LPPROVIDOR_INFO_1A = ^_PROVIDOR_INFO_1A;

     _PROVIDOR_INFO_1W = record
          pName : LPWSTR;
          pEnvironment : LPWSTR;
          pDLLName : LPWSTR;
       end;
     PROVIDOR_INFO_1W = _PROVIDOR_INFO_1W;
     PPROVIDOR_INFO_1W = ^_PROVIDOR_INFO_1W;
     LPPROVIDOR_INFO_1W = ^_PROVIDOR_INFO_1W;

     _PROVIDOR_INFO_2A = record
          pOrder : LPSTR;
       end;
     PROVIDOR_INFO_2A = _PROVIDOR_INFO_2A;
     PPROVIDOR_INFO_2A = ^_PROVIDOR_INFO_2A;
     LPROVIDOR_INFO_2A = ^_PROVIDOR_INFO_2A;

     _PROVIDOR_INFO_2W = record
          pOrder : LPWSTR;
       end;
     PROVIDOR_INFO_2W = _PROVIDOR_INFO_2W;
     PPROVIDOR_INFO_2W = ^_PROVIDOR_INFO_2W;
     LPROVIDOR_INFO_2W = ^_PROVIDOR_INFO_2W;

  function AbortPrinter(_para1:HANDLE):BOOL;stdcall; external External_library name 'AbortPrinter';

  function OpenPrinterA(_para1:LPSTR; _para2:PHANDLE; _para3:LPPRINTER_DEFAULTSA):BOOL;stdcall; external External_library name 'OpenPrinterA';

  function OpenPrinterW(_para1:LPWSTR; _para2:PHANDLE; _para3:LPPRINTER_DEFAULTSW):BOOL;stdcall; external External_library name 'OpenPrinterW';

  function PrinterMessageBox(_para1:HANDLE; _para2:DWORD; _para3:HWND; _para4:LPSTR; _para5:LPSTR;
             _para6:DWORD):DWORD;stdcall; external External_library name 'PrinterMessageBoxA';

  function PrinterMessageBox(_para1:HANDLE; _para2:DWORD; _para3:HWND; _para4:LPWSTR; _para5:LPWSTR;
             _para6:DWORD):DWORD;stdcall; external External_library name 'PrinterMessageBoxW';

  function PrinterMessageBoxA(_para1:HANDLE; _para2:DWORD; _para3:HWND; _para4:LPSTR; _para5:LPSTR;
             _para6:DWORD):DWORD;stdcall; external External_library name 'PrinterMessageBoxA';

  function PrinterMessageBoxW(_para1:HANDLE; _para2:DWORD; _para3:HWND; _para4:LPWSTR; _para5:LPWSTR;
             _para6:DWORD):DWORD;stdcall; external External_library name 'PrinterMessageBoxW';

  type
     PROVIDOR_INFO_2 = PROVIDOR_INFO_2A;
     PPROVIDOR_INFO_2 = ^PROVIDOR_INFO_2;
     LPROVIDOR_INFO_2 = ^PROVIDOR_INFO_2;

implementation


end.
