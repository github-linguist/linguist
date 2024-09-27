// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/win32/types.c3
// Permalink: https://github.com/c3lang/c3c/blob/81c93e3/lib/std/os/win32/types.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::win32;

def Win32_BOOL = int;
def Win32_BOOLEAN = Win32_BYTE;
def Win32_BYTE = char;
def Win32_CCHAR = cinterop::CChar;
def Win32_CHAR = cinterop::CChar;
def Win32_COLORREF = Win32_DWORD;
def Win32_DWORD = uint;
def Win32_DWORDLONG = ulong;
def Win32_DWORD_PTR = Win32_ULONG_PTR;
def Win32_DWORD32 = uint;
def Win32_DWORD64 = ulong;
def Win32_FLOAT = float;
def Win32_HACCEL = Win32_HANDLE;
def Win32_HALF_PTR = int;
def Win32_HANDLE = Win32_PVOID;
def Win32_HBITMAP = Win32_HANDLE;
def Win32_HBRUSH = Win32_HANDLE;
def Win32_HCOLORSPACE = Win32_HANDLE;
def Win32_HCONV = Win32_HANDLE;
def Win32_HCONVLIST = Win32_HANDLE;
def Win32_HCURSOR = Win32_HICON;
def Win32_HDC = Win32_HANDLE;
def Win32_HDDEDATA = Win32_HANDLE;
def Win32_HDESK = Win32_HANDLE;
def Win32_HDROP = Win32_HANDLE;
def Win32_HDWP = Win32_HANDLE;
def Win32_HFILE = int;
def Win32_HFONT = Win32_HANDLE;
def Win32_HGDIOBJ = Win32_HANDLE;
def Win32_HGLOBAL = Win32_HANDLE;
def Win32_HHOOK = Win32_HANDLE;
def Win32_HICON = Win32_HANDLE;
def Win32_HINSTANCE = Win32_HANDLE;
def Win32_HKEY = Win32_HANDLE;
def Win32_HKL = Win32_HANDLE;
def Win32_HLOCAL = Win32_HANDLE;
def Win32_HMENU = Win32_HANDLE;
def Win32_HMETAFILE = Win32_HANDLE;
def Win32_HMODULE = Win32_HANDLE;
def Win32_HMONITOR = Win32_HANDLE;
def Win32_HPALETTE = Win32_HANDLE;
def Win32_HPEN = Win32_HANDLE;
def Win32_HRESULT = Win32_LONG;
def Win32_HRGN = Win32_HANDLE;
def Win32_HRSRC = Win32_HANDLE;
def Win32_HSZ = Win32_HANDLE;
def Win32_HWINSTA = Win32_HANDLE;
def Win32_HWND = Win32_HANDLE;
def Win32_INT = int;
def Win32_INT_PTR = iptr;
def Win32_INT8 = ichar;
def Win32_INT16 = short;
def Win32_INT32 = int;
def Win32_INT64 = long;
def Win32_LANGID = Win32_WORD;
def Win32_LCID = Win32_DWORD;
def Win32_LCTYPE = Win32_DWORD;
def Win32_LGRPID = Win32_DWORD;
def Win32_LONG = int;
def Win32_LONGLONG = long;
def Win32_LONG_PTR = iptr;
def Win32_LONG32 = int;
def Win32_LONG64 = long;
def Win32_LPARAM = Win32_LONG_PTR;
def Win32_LPBOOL = Win32_BOOL*;
def Win32_LPBYTE = Win32_BYTE*;
def Win32_LPCOLORREF = Win32_DWORD*;
def Win32_LPCSTR = Win32_CCHAR*;
def Win32_LPCTSTR = Win32_LPCWSTR;
def Win32_LPCVOID = void*;
def Win32_LPCWSTR = Win32_WCHAR*;
def Win32_LPDWORD = Win32_DWORD*;
def Win32_LPHANDLE = Win32_HANDLE*;
def Win32_LPINT = int*;
def Win32_LPLONG = int*;
def Win32_LPSTR = Win32_CCHAR*;
def Win32_LPTSTR = Win32_LPWSTR;
def Win32_LPVOID = void*;
def Win32_LPWORD = Win32_WORD*;
def Win32_LPWSTR = Win32_WCHAR*;
def Win32_LRESULT = Win32_LONG_PTR;
def Win32_PBOOL = Win32_BOOL*;
def Win32_PBOOLEAN = Win32_BOOLEAN*;
def Win32_PBYTE = Win32_BYTE*;
def Win32_PCHAR = Win32_CHAR*;
def Win32_PCSTR = Win32_CHAR*;
def Win32_PCTSTR = Win32_LPCWSTR;
def Win32_PCUNICODE_STRING = Win32_UNICODE_STRING*;
def Win32_PCWSTR = WString;
def Win32_PDWORD = Win32_DWORD*;
def Win32_PDWORDLONG = Win32_DWORDLONG*;
def Win32_PDWORDPTR = Win32_DWORD_PTR*;
def Win32_PDWORD32 = Win32_DWORD32*;
def Win32_PDWORD64 = Win32_DWORD64*;
def Win32_PFLOAT = Win32_FLOAT*;
def Win32_PHALFPTR = Win32_HALF_PTR*;
def Win32_PHANDLE = Win32_HANDLE*;
def Win32_PHKEY = Win32_HKEY*;
def Win32_PINT = int*;
def Win32_PINTPTR = Win32_INT_PTR*;
def Win32_PINT8 = Win32_INT8*;
def Win32_PINT16 = Win32_INT16*;
def Win32_PINT32 = Win32_INT32*;
def Win32_PINT64 = Win32_INT64*;
def Win32_PLCID = Win32_PDWORD;
def Win32_PLONG = Win32_LONG*;
def Win32_PLONGLONG = Win32_LONGLONG*;
def Win32_PLONG_PTR = Win32_LONG_PTR*;
def Win32_PLONG32 = Win32_LONG32*;
def Win32_PLONG64 = Win32_LONG64*;
def Win32_POINTER_32 = uint;
def Win32_POINTER_64 = uptr;
def Win32_POINTER_SIGNED = iptr;
def Win32_POINTER_UNSIGNED = uptr;
def Win32_PSHORT = Win32_SHORT*;
def Win32_PSIZE_T = usz*;
def Win32_PSSIZE_T = isz*;
def Win32_PSTR = Win32_CHAR*;
def Win32_PTBYTE = Win32_TBYTE*;
def Win32_PTCHAR = Win32_TCHAR*;
def Win32_PTSTR = Win32_LPWSTR;
def Win32_PUCHAR = Win32_UCHAR*;
def Win32_PUHALFPTR = Win32_UHALF_PTR*;
def Win32_PUINT = Win32_UINT*;
def Win32_PUINTPTR = Win32_UINT_PTR*;
def Win32_PUINT8 = Win32_UINT8*;
def Win32_PUINT16 = Win32_UINT16*;
def Win32_PUINT32 = Win32_UINT32*;
def Win32_PUINT64 = Win32_UINT64*;
def Win32_PULONG = Win32_ULONG*;
def Win32_PULONGLONG = Win32_ULONGLONG*;
def Win32_PULONG_PTR = Win32_ULONG_PTR*;
def Win32_PULONG32 = Win32_ULONG32*;
def Win32_PULONG64 = Win32_ULONG64*;
def Win32_PUNICODE_STRING = Win32_UNICODE_STRING*;
def Win32_PUSHORT = Win32_USHORT*;
def Win32_PVOID = void*;
def Win32_PWCHAR = Win32_WCHAR*;
def Win32_PWORD = Win32_WORD*;
def Win32_PWSTR = Win32_WCHAR*;
def Win32_QWORD = ulong;
def Win32_SC_HANDLE = Win32_HANDLE;
def Win32_SC_LOCK = Win32_LPVOID;
def Win32_SERVICE_STATUS_HANDLE = Win32_HANDLE;
def Win32_SHORT = short;
def Win32_SIZE_T = usz;
def Win32_SOCKET = Win32_HANDLE;
def Win32_SSIZE_T = isz;
def Win32_TBYTE = Win32_WCHAR;
def Win32_TCHAR = Win32_WCHAR;
def Win32_UCHAR = char;
def Win32_UHALF_PTR = uint;
def Win32_UINT = uint;
def Win32_UINT_PTR = uptr;
def Win32_UINT8 = char;
def Win32_UINT16 = ushort;
def Win32_UINT32 = uint;
def Win32_UINT64 = ulong;
def Win32_ULONG = uint;
def Win32_ULONGLONG = ulong;
def Win32_ULONG_PTR = ulong;
def Win32_ULONG32 = uint;
def Win32_ULONG64 = ulong;
def Win32_USHORT = ushort;
def Win32_USN = Win32_LONGLONG;
def Win32_WCHAR = Char16;
def Win32_WORD = ushort;
def Win32_WPARAM = Win32_UINT_PTR;

const INVALID_HANDLE_VALUE = (Win32_HANDLE)(uptr)-1;

struct Win32_UNICODE_STRING
{
	Win32_USHORT length;
	Win32_USHORT maximum_length;
	Win32_PWSTR buffer;
}

union Win32_LARGE_INTEGER
{
	struct
	{
		Win32_DWORD lowPart;
		Win32_LONG  highPart;
	}
	ulong quadPart;
}

distinct Win32_CRITICAL_SECTION = ulong[5];

struct Win32_SECURITY_ATTRIBUTES
{
	Win32_DWORD  nLength;
	Win32_LPVOID lpSecurityDescriptor;
	Win32_BOOL   bInheritHandle;
}



def Win32_LPSECURITY_ATTRIBUTES = Win32_SECURITY_ATTRIBUTES*;
def Win32_PSECURITY_ATTRIBUTES = Win32_SECURITY_ATTRIBUTES*;

struct Win32_STARTUPINFOW
{
	Win32_DWORD  cb;
	Win32_LPWSTR lpReserved;
	Win32_LPWSTR lpDesktop;
	Win32_LPWSTR lpTitle;
	Win32_DWORD  dwX;
	Win32_DWORD  dwY;
	Win32_DWORD  dwXSize;
	Win32_DWORD  dwYSize;
	Win32_DWORD  dwXCountChars;
	Win32_DWORD  dwYCountChars;
	Win32_DWORD  dwFillAttribute;
	Win32_DWORD  dwFlags;
	Win32_WORD   wShowWindow;
	Win32_WORD   cbReserved2;
	Win32_LPBYTE lpReserved2;
	Win32_HANDLE hStdInput;
	Win32_HANDLE hStdOutput;
	Win32_HANDLE hStdError;
}

struct Win32_OVERLAPPED
{
	Win32_ULONG_PTR internal;
	Win32_ULONG_PTR internalHigh;
	union {
		struct {
			Win32_DWORD offset;
			Win32_DWORD offsetHigh;
		}
		Win32_PVOID pointer;
	}
	Win32_HANDLE hEvent;
}

def Win32_LPOVERLAPPED = Win32_OVERLAPPED*;
def Win32_LPSTARTUPINFOW = Win32_STARTUPINFOW*;

struct Win32_STARTUPINFOEXW
{
	inline Win32_STARTUPINFOW win32_StartupInfo;
	Win32_LPPROC_THREAD_ATTRIBUTE_LIST lpAttributeList;
}

def Win32_LPPROC_THREAD_ATTRIBUTE_LIST = void*;
def Win32_LPSTARTUPINFOEXW = Win32_STARTUPINFOEXW*;

struct Win32_FILETIME
{
	Win32_DWORD dwLowDateTime;
	Win32_DWORD dwHighDateTime;
}

struct Win32_PROCESS_INFORMATION
{
	Win32_HANDLE hProcess;
	Win32_HANDLE hThread;
	Win32_DWORD  dwProcessId;
	Win32_DWORD  dwThreadId;
}

def Win32_PPROCESS_INFORMATION = Win32_PROCESS_INFORMATION*;
def Win32_LPPROCESS_INFORMATION = Win32_PROCESS_INFORMATION*;

struct Win32_SYSTEM_INFO
{
	union
	{
		Win32_DWORD dwOemId;
		struct
		{
			Win32_WORD wProcessorArchitecture;
			Win32_WORD wReserved;
		}
	}
	Win32_DWORD         dwPageSize;
	Win32_LPVOID        lpMinimumApplicationAddress;
	Win32_LPVOID        lpMaximumApplicationAddress;
	Win32_DWORD_PTR     dwActiveProcessorMask;
	Win32_DWORD         dwNumberOfProcessors;
	Win32_DWORD         dwProcessorType;
	Win32_DWORD         dwAllocationGranularity;
	Win32_WORD          wProcessorLevel;
	Win32_WORD          wProcessorRevision;
}

def Win32_LPSYSTEM_INFO = Win32_SYSTEM_INFO*;

struct Win32_MODULEINFO
{
	Win32_LPVOID lpBaseOfDll;
	Win32_DWORD  sizeOfImage;
	Win32_LPVOID entryPoint;
}

struct Win32_IMAGEHLP_LINE64
{
	Win32_DWORD   sizeOfStruct;
	Win32_PVOID   key;
	Win32_DWORD   lineNumber;
	Win32_PCHAR   fileName;
	Win32_DWORD64 address;
}

enum Win32_SYM_TYPE
{
	SYM_NONE,
	SYM_COFF,
	SYM_CV,
	SYM_PDB,
	SYM_EXPORT,
	SYM_DEFERRED,
	SYM_SYM,
	SYM_DIA,
	SYM_VIRTUAL
}

struct Win32_GUID
{
	CULong data1;
    CUShort data2;
    CUShort data3;
    char[8] data4;
}

struct Win32_IMAGEHLP_MODULE64
{
    Win32_DWORD     sizeOfStruct;
    Win32_DWORD64   baseOfImage;
    Win32_DWORD     imageSize;
    Win32_DWORD     timeDateStamp;
    Win32_DWORD     checkSum;
    Win32_DWORD     numSyms;
    Win32_SYM_TYPE  symType;
    Win32_CHAR[32]  moduleName;
    Win32_CHAR[256] imageName;
    Win32_CHAR[256] loadedImageName;
    Win32_CHAR[256] loadedPdbName;
    Win32_DWORD     cVSig;
    Win32_CHAR**    cVData;
    Win32_DWORD     pdbSig;
    Win32_GUID      pdbSig70;
    Win32_DWORD     pdbAge;
    Win32_BOOL      pdbUnmatched;
    Win32_BOOL      dbgUnmatched;
    Win32_BOOL      lineNumbers;
    Win32_BOOL      globalSymbols;
    Win32_BOOL      typeInfo;
    Win32_BOOL      sourceIndexed;
    Win32_BOOL      publics;
    Win32_DWORD     machineType;
    Win32_DWORD     reserved;
}

def Win32_PIMAGEHLP_MODULE64 = Win32_IMAGEHLP_MODULE64*;

struct Win32_ARM64_NT_CONTEXT @align(16)
{
	Win32_DWORD   contextFlags;
	Win32_DWORD   cpsr;
    union
    {
		struct
		{
            Win32_DWORD64 x0;
            Win32_DWORD64 x1;
            Win32_DWORD64 x2;
            Win32_DWORD64 x3;
            Win32_DWORD64 x4;
            Win32_DWORD64 x5;
            Win32_DWORD64 x6;
            Win32_DWORD64 x7;
            Win32_DWORD64 x8;
            Win32_DWORD64 x9;
            Win32_DWORD64 x10;
            Win32_DWORD64 x11;
            Win32_DWORD64 x12;
            Win32_DWORD64 x13;
            Win32_DWORD64 x14;
            Win32_DWORD64 x15;
            Win32_DWORD64 x16;
            Win32_DWORD64 x17;
            Win32_DWORD64 x18;
            Win32_DWORD64 x19;
            Win32_DWORD64 x20;
            Win32_DWORD64 x21;
            Win32_DWORD64 x22;
            Win32_DWORD64 x23;
            Win32_DWORD64 x24;
            Win32_DWORD64 x25;
            Win32_DWORD64 x26;
            Win32_DWORD64 x27;
            Win32_DWORD64 x28;
            Win32_DWORD64 fp;
            Win32_DWORD64 lr;
        }
        Win32_DWORD64[31] x;
    }
	Win32_DWORD64                           sp;
	Win32_DWORD64                           pc;
	Win32_ARM64_NT_NEON128[32]              v;
	Win32_DWORD                             fpcr;
	Win32_DWORD                             fpsr;
	Win32_DWORD[ARM64_MAX_BREAKPOINTS]      bcr;
	Win32_DWORD64[ARM64_MAX_BREAKPOINTS]    bvr;
	Win32_DWORD[ARM64_MAX_WATCHPOINTS]      wcr;
	Win32_DWORD64[ARM64_MAX_WATCHPOINTS]    wvr;
}
const ARM64_MAX_BREAKPOINTS = 8;
const ARM64_MAX_WATCHPOINTS = 2;

struct Win32_ARM64_NT_NEON128
{
    Win32_ULONGLONG low;
    Win32_LONGLONG  high;
}

struct Win32_XMM_SAVE_AREA32
{
	Win32_USHORT    controlWord;
	Win32_USHORT    statusWord;
	Win32_UCHAR     tagWord;
	Win32_UCHAR     reserved1;
	Win32_USHORT    errorOpcode;
	Win32_ULONG     errrorOffset;
	Win32_USHORT    errorSelector;
	Win32_USHORT    reserved2;
	Win32_ULONG     dataOffset;
	Win32_USHORT    dataSelector;
	Win32_USHORT    reserved3;
	Win32_ULONG     mxCsr;
	Win32_ULONG     mxCsr_Mask;
	Win32_M128A[8]  floatRegisters;
	Win32_M128A[16] xmmRegisters;
	Win32_UCHAR[96] reserved4;
}

struct Win32_AMD64_CONTEXT @align(16)
{
	Win32_DWORD64 p1Home;
	Win32_DWORD64 p2Home;
	Win32_DWORD64 p3Home;
	Win32_DWORD64 p4Home;
	Win32_DWORD64 p5Home;
	Win32_DWORD64 p6Home;
	Win32_DWORD   contextFlags;
	Win32_DWORD   mxCsr;
	Win32_WORD    segCs;
	Win32_WORD    segDs;
	Win32_WORD    segEs;
	Win32_WORD    segFs;
	Win32_WORD    segGs;
	Win32_WORD    segSs;
	Win32_DWORD   eFlags;
	Win32_DWORD64 dr0;
	Win32_DWORD64 dr1;
	Win32_DWORD64 dr2;
	Win32_DWORD64 dr3;
	Win32_DWORD64 dr6;
	Win32_DWORD64 dr7;
	Win32_DWORD64 rax;
	Win32_DWORD64 rcx;
	Win32_DWORD64 rdx;
	Win32_DWORD64 rbx;
	Win32_DWORD64 rsp;
	Win32_DWORD64 rbp;
	Win32_DWORD64 rsi;
	Win32_DWORD64 rdi;
	Win32_DWORD64 r8;
	Win32_DWORD64 r9;
	Win32_DWORD64 r10;
	Win32_DWORD64 r11;
	Win32_DWORD64 r12;
	Win32_DWORD64 r13;
	Win32_DWORD64 r14;
	Win32_DWORD64 r15;
	Win32_DWORD64 rip;
    union
    {
        Win32_XMM_SAVE_AREA32   fltSave;
        //Win32_NEON128[16]       q;
        Win32_ULONGLONG[32]     d;
        struct
        {
            Win32_M128A[2]  header;
            Win32_M128A[8]  legacy;
            Win32_M128A     xmm0;
            Win32_M128A     xmm1;
            Win32_M128A     xmm2;
            Win32_M128A     xmm3;
            Win32_M128A     xmm4;
            Win32_M128A     xmm5;
            Win32_M128A     xmm6;
            Win32_M128A     xmm7;
            Win32_M128A     xmm8;
            Win32_M128A     xmm9;
            Win32_M128A     xmm10;
            Win32_M128A     xmm11;
            Win32_M128A     xmm12;
            Win32_M128A     xmm13;
            Win32_M128A     xmm14;
            Win32_M128A     xmm15;
        }
        Win32_DWORD[32]     s;
    }
	Win32_M128A[26]         vectorRegister;
	Win32_DWORD64           vectorControl;
	Win32_DWORD64           debugControl;
	Win32_DWORD64           lastBranchToRip;
	Win32_DWORD64           lastBranchFromRip;
	Win32_DWORD64           lastExceptionToRip;
	Win32_DWORD64           lastExceptionFromRip;
}

const CONTEXT_AMD64 = 0x00100000;
const CONTEXT_AMD64_CONTROL = CONTEXT_AMD64 | 0x0001;
const CONTEXT_AMD64_INTEGER = CONTEXT_AMD64 | 0x0002;
const CONTEXT_AMD64_SEGMENTS = CONTEXT_AMD64 | 0x0004;
const CONTEXT_AMD64_FLOATING_POINT = CONTEXT_AMD64 | 0x0008;
const CONTEXT_AMD64_DEBUG_REGISTERS = CONTEXT_AMD64 | 0x0010;
const CONTEXT_AMD64_FULL = CONTEXT_AMD64_CONTROL | CONTEXT_AMD64_INTEGER | CONTEXT_AMD64_FLOATING_POINT;
const CONTEXT_AMD64_ALL = CONTEXT_AMD64_CONTROL | CONTEXT_AMD64_INTEGER | CONTEXT_AMD64_SEGMENTS | CONTEXT_AMD64_FLOATING_POINT | CONTEXT_AMD64_DEBUG_REGISTERS;

def CONTEXT_CONTROL = CONTEXT_AMD64_CONTROL;
def CONTEXT_FULL = CONTEXT_AMD64_FULL;
def CONTEXT_ALL = CONTEXT_AMD64_ALL;

def Win32_CONTEXT = Win32_AMD64_CONTEXT;
def Win32_PCONTEXT = Win32_CONTEXT*;

struct Win32_M128A @align(16)
{
    Win32_ULONGLONG low;
    Win32_LONGLONG  high;
}

struct Win32_IMAGE_DATA_DIRECTORY
{
    Win32_ULONG   virtualAddress;
    Win32_ULONG   size;
}

struct Win32_IMAGE_OPTIONAL_HEADER64
{
	Win32_WORD                 magic;
	Win32_BYTE                 majorLinkerVersion;
	Win32_BYTE                 minorLinkerVersion;
	Win32_DWORD                sizeOfCode;
	Win32_DWORD                sizeOfInitializedData;
	Win32_DWORD                sizeOfUninitializedData;
	Win32_DWORD                addressOfEntryPoint;
	Win32_DWORD                baseOfCode;
	Win32_ULONGLONG            imageBase;
	Win32_DWORD                sectionAlignment;
	Win32_DWORD                fileAlignment;
	Win32_WORD                 majorOperatingSystemVersion;
	Win32_WORD                 minorOperatingSystemVersion;
	Win32_WORD                 majorImageVersion;
	Win32_WORD                 minorImageVersion;
	Win32_WORD                 majorSubsystemVersion;
	Win32_WORD                 minorSubsystemVersion;
	Win32_DWORD                win32VersionValue;
	Win32_DWORD                sizeOfImage;
	Win32_DWORD                sizeOfHeaders;
	Win32_DWORD                checkSum;
	Win32_WORD                 subsystem;
	Win32_WORD                 dllCharacteristics;
	Win32_ULONGLONG            sizeOfStackReserve;
	Win32_ULONGLONG            sizeOfStackCommit;
	Win32_ULONGLONG            sizeOfHeapReserve;
	Win32_ULONGLONG            sizeOfHeapCommit;
	Win32_DWORD                loaderFlags;
	Win32_DWORD                numberOfRvaAndSizes;
	Win32_IMAGE_DATA_DIRECTORY[16] dataDirectory;
}

def Win32_PIMAGE_OPTIONAL_HEADER64 = Win32_IMAGE_OPTIONAL_HEADER64*;
struct Win32_IMAGE_FILE_HEADER
{
    Win32_WORD      machine;
    Win32_WORD      numberOfSections;
    Win32_DWORD     timeDateStamp;
    Win32_DWORD     pointerToSymbolTable;
    Win32_DWORD     numberOfSymbols;
    Win32_WORD      sizeOfOptionalHeader;
    Win32_WORD      characteristics;
}

def Win32_PIMAGE_FILE_HEADER = Win32_IMAGE_FILE_HEADER*;

struct Win32_IMAGE_NT_HEADERS
{
    Win32_DWORD signature;
    Win32_IMAGE_FILE_HEADER fileHeader;
    Win32_IMAGE_OPTIONAL_HEADER64 optionalHeader;
}

def Win32_PIMAGE_NT_HEADERS = Win32_IMAGE_NT_HEADERS*;

struct Win32_SYMBOL_INFO
{
	Win32_ULONG         sizeOfStruct;
	Win32_ULONG         typeIndex;
	Win32_ULONG64[2]    reserved;
	Win32_ULONG         index;
	Win32_ULONG         size;
	Win32_ULONG64       modBase;
	Win32_ULONG         flags;
	Win32_ULONG64       value;
	Win32_ULONG64       address;
	Win32_ULONG         register;
	Win32_ULONG         scope;
	Win32_ULONG         tag;
	Win32_ULONG         nameLen;
	Win32_ULONG         maxNameLen;
	Win32_CHAR[1]       name;
}

def Win32_PSYMBOL_INFO = Win32_SYMBOL_INFO*;

struct Win32_MODLOAD_DATA
{
	Win32_DWORD ssize;
	Win32_DWORD ssig;
	Win32_PVOID data;
	Win32_DWORD size;
	Win32_DWORD flags;
}

enum Win32_ADDRESS_MODE
{
	ADDR_MODE_1616,
    ADDR_MODE_1632,
    ADDR_MODE_REAL,
    ADDR_MODE_FLAT,
}

struct Win32_ADDRESS64
{
	Win32_DWORD64       offset;
	Win32_WORD          segment;
	Win32_ADDRESS_MODE  mode;
}

struct Win32_KDHELP64
{
    Win32_DWORD64       thread;
    Win32_DWORD         thCallbackStack;
    Win32_DWORD         thCallbackBStore;
    Win32_DWORD         nextCallback;
    Win32_DWORD         framePointer;
    Win32_DWORD64       kiCallUserMode;
    Win32_DWORD64       keUserCallbackDispatcher;
    Win32_DWORD64       systemRangeStart;
    Win32_DWORD64       kiUserExceptionDispatcher;
    Win32_DWORD64       stackBase;
    Win32_DWORD64       stackLimit;
    Win32_DWORD         buildVersion;
    Win32_DWORD         retpolineStubFunctionTableSize;
    Win32_DWORD64       retpolineStubFunctionTable;
    Win32_DWORD         retpolineStubOffset;
    Win32_DWORD         retpolineStubSize;
    Win32_DWORD64[2]    reserved0;
}

struct Win32_STACKFRAME64
{
	Win32_ADDRESS64     addrPC;
	Win32_ADDRESS64     addrReturn;
	Win32_ADDRESS64     addrFrame;
	Win32_ADDRESS64     addrStack;
	Win32_ADDRESS64     addrBStore;
	Win32_PVOID         funcTableEntry;
	Win32_DWORD64[4]    params;
	Win32_BOOL          far;
	Win32_BOOL          virtual;
	Win32_DWORD64[3]    reserved;
	Win32_KDHELP64      kdHelp;
}

def Win32_PREAD_PROCESS_MEMORY_ROUTINE64 = fn Win32_BOOL(Win32_HANDLE hProcess, Win32_DWORD64 qwBaseAddress, Win32_PVOID lpBuffer, Win32_DWORD nSize, Win32_LPDWORD lpNumberOfBytesRead);
def Win32_PFUNCTION_TABLE_ACCESS_ROUTINE64 = fn Win32_PVOID(Win32_HANDLE ahProcess, Win32_DWORD64 addrBase);
def Win32_PGET_MODULE_BASE_ROUTINE64 = fn Win32_DWORD64(Win32_HANDLE hProcess, Win32_DWORD64 address);
def Win32_PTRANSLATE_ADDRESS_ROUTINE64 = fn Win32_DWORD64(Win32_HANDLE hProcess, Win32_HANDLE hThread, Win32_LPADDRESS64 lpaddr);
def Win32_PKDHELP64 = Win32_KDHELP64*;
def Win32_LPADDRESS64 = Win32_ADDRESS64*;
def Win32_LPSTACKFRAME64 = Win32_STACKFRAME64*;
def Win32_PMODLOAD_DATA = Win32_MODLOAD_DATA*;
def Win32_PIMAGEHLP_LINE64 = Win32_IMAGEHLP_LINE64*;
def Win32_LPMODULEINFO = Win32_MODULEINFO*;

