
unit cwindirs;




interface

uses
  windows,
  strings;

Const
  CSIDL_PROGRAMS                = $0002;
  CSIDL_PERSONAL                = $0005;
  CSIDL_FAVORITES               = $0006;
  CSIDL_STARTUP                 = $0007;
  CSIDL_RECENT                  = $0008;
  CSIDL_SENDTO                  = $0009;
  CSIDL_STARTMENU               = $000B;
  CSIDL_MYMUSIC                 = $000D;
  CSIDL_MYVIDEO                 = $000E;
  CSIDL_DESKTOPDIRECTORY        = $0010;
  CSIDL_NETHOOD                 = $0013;
  CSIDL_TEMPLATES               = $0015;
  CSIDL_COMMON_STARTMENU        = $0016;
  CSIDL_COMMON_PROGRAMS         = $0017;
  CSIDL_COMMON_STARTUP          = $0018;
  CSIDL_COMMON_DESKTOPDIRECTORY = $0019;
  CSIDL_APPDATA                 = $001A;
  CSIDL_PRINTHOOD               = $001B;
  CSIDL_LOCAL_APPDATA           = $001C;
  CSIDL_COMMON_FAVORITES        = $001F;
  CSIDL_INTERNET_CACHE          = $0020;
  CSIDL_COOKIES                 = $0021;
  CSIDL_HISTORY                 = $0022;
  CSIDL_COMMON_APPDATA          = $0023;
  CSIDL_WINDOWS                 = $0024;
  CSIDL_SYSTEM                  = $0025;
  CSIDL_PROGRAM_FILES           = $0026;
  CSIDL_MYPICTURES              = $0027;
  CSIDL_PROFILE                 = $0028;
  CSIDL_PROGRAM_FILES_COMMON    = $002B;
  CSIDL_COMMON_TEMPLATES        = $002D;
  CSIDL_COMMON_DOCUMENTS        = $002E;
  CSIDL_COMMON_ADMINTOOLS       = $002F;
  CSIDL_ADMINTOOLS              = $0030;
  CSIDL_COMMON_MUSIC            = $0035;
  CSIDL_COMMON_PICTURES         = $0036;
  CSIDL_COMMON_VIDEO            = $0037;
  CSIDL_CDBURN_AREA             = $003B;
  CSIDL_PROFILES                = $003E;

  CSIDL_FLAG_CREATE             = $8000;

Function GetWindowsSpecialDir(ID :  Integer) : String;

implementation

uses
  sysutils;

Type
  PFNSHGetFolderPath = Function(Ahwnd: HWND; Csidl: Integer; Token: THandle; Flags: DWord; Path: PChar): HRESULT; stdcall;


var
  SHGetFolderPath : PFNSHGetFolderPath = Nil;
  CFGDLLHandle : THandle = 0;

Procedure InitDLL;

Var
  pathBuf: array[0..MAX_PATH-1] of char;
  pathLength: Integer;
begin
  { Load shfolder.dll using a full path, in order to prevent spoofing (Mantis #18185)
    Don't bother loading shell32.dll because shfolder.dll itself redirects SHGetFolderPath
    to shell32.dll whenever possible. }
  pathLength:=GetSystemDirectory(pathBuf, MAX_PATH);
  if (pathLength>0) and (pathLength<MAX_PATH-14) then
  begin
    StrLCopy(@pathBuf[pathLength],'\shfolder.dll',MAX_PATH-pathLength-1);
    CFGDLLHandle:=LoadLibrary(pathBuf);

    if (CFGDLLHandle<>0) then
    begin
      Pointer(ShGetFolderPath):=GetProcAddress(CFGDLLHandle,'SHGetFolderPathA');
      If @ShGetFolderPath=nil then
      begin
        FreeLibrary(CFGDLLHandle);
        CFGDllHandle:=0;
      end;
    end;
  end;
  If (@ShGetFolderPath=Nil) then
    Raise Exception.Create('Could not determine SHGetFolderPath Function');
end;

Function GetWindowsSpecialDir(ID :  Integer) : String;

Var
  APath : Array[0..MAX_PATH] of char;

begin
  Result:='';
  if (CFGDLLHandle=0) then
    InitDLL;
  If (SHGetFolderPath<>Nil) then
    begin
    if SHGetFolderPath(0,ID or CSIDL_FLAG_CREATE,0,0,@APATH[0])=S_OK then
      Result:=IncludeTrailingPathDelimiter(StrPas(@APath[0]));
    end;
end;

Initialization
Finalization
  if CFGDLLHandle<>0 then
   FreeLibrary(CFGDllHandle);
end.

