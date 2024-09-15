// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/win32/process.c3
// Permalink: https://github.com/c3lang/c3c/blob/ed5d338/lib/std/os/win32/process.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::win32 @if(env::WIN32);
import std::thread, std::os::backtrace;

const Win32_DWORD STARTF_USESTDHANDLES = 0x00000100;
const Win32_DWORD CREATE_NO_WINDOW = 0x08000000;
const Win32_DWORD CREATE_PROTECTED_PROCESS = 0x00040000;
const Win32_DWORD CREATE_UNICODE_ENVIRONMENT = 0x00000400;
const uint WAIT_OBJECT_0 = 0;
const uint WAIT_ABANDONED = 128;
const uint WAIT_IO_COMPLETION = 192;
const uint WAIT_FAILED = (uint)-1;
const Win32_DWORD HANDLE_FLAG_INHERIT = 1;
const Win32_DWORD HANDLE_FLAG_PROTECT_FROM_CLOSE = 2;
const uint INFINITE = (uint)-1;
const Win32_DWORD PIPE_ACCESS_DUPLEX = 0x00000003;
const Win32_DWORD PIPE_ACCESS_INBOUND = 0x00000001;
const Win32_DWORD PIPE_ACCESS_OUTBOUND = 0x00000002;
const Win32_DWORD FILE_FLAG_FIRST_PIPE_INSTANCE = 0x00080000;
const Win32_DWORD FILE_FLAG_WRITE_THROUGH = 0x80000000;
const Win32_DWORD FILE_FLAG_OVERLAPPED = 0x40000000;
const Win32_DWORD WRITE_DAC = 0x00040000;
const Win32_DWORD WRITE_OWNER = 0x00080000;
const Win32_DWORD ACCESS_SYSTEM_SECURITY = 0x01000000;
const Win32_DWORD PIPE_TYPE_BYTE = 0;
const Win32_DWORD PIPE_TYPE_MESSAGE = 4;
const Win32_DWORD PIPE_READMODE_BYTE = 0;
const Win32_DWORD PIPE_READMODE_MESSAGE = 2;
const Win32_DWORD PIPE_WAIT = 0;
const Win32_DWORD PIPE_NOWAIT = 1;
const Win32_DWORD PIPE_ACCEPT_REMOTE_CLIENTS = 0;
const Win32_DWORD PIPE_REJECT_REMOTE_CLIENTS = 8;
const SYMOPT_CASE_INSENSITIVE = 0x01;
const SYMOPT_UNDNAME = 0x02;
const SYMOPT_DEFERRED_LOADS = 0x04;
const SYMOPT_NO_CPP = 0x08;
const SYMOPT_LOAD_LINES = 0x10;
const SYMOPT_OMAP_FIND_NEAREST = 0x20;
const SYMOPT_LOAD_ANYTHING = 0x40;
const SYMOPT_IGNORE_CVREC = 0x80;

const IMAGE_FILE_MACHINE_UNKNOWN = 0;
const IMAGE_FILE_MACHINE_TARGET_HOST = 1;
const IMAGE_FILE_MACHINE_I386 = 0x014c;
const IMAGE_FILE_MACHINE_IA64 = 0x0200;
const IMAGE_FILE_MACHINE_ARM64 = 0xAA64;
const IMAGE_FILE_MACHINE_AMD64 = 0x8664;
const UNDNAME_COMPLETE = 0x0000;

extern fn void initializeCriticalSection(Win32_CRITICAL_SECTION* section) @extern("InitializeCriticalSection");
extern fn void deleteCriticalSection(Win32_CRITICAL_SECTION* section) @extern("DeleteCriticalSection");
extern fn Win32_HANDLE createMutex(void*, Win32_BOOL, void*) @extern("CreateMutexA");
extern fn Win32_BOOL releaseMutex(Win32_HANDLE) @extern("ReleaseMutex");
extern fn void enterCriticalSection(Win32_CRITICAL_SECTION* section) @extern("EnterCriticalSection");
extern fn void leaveCriticalSection(Win32_CRITICAL_SECTION* section) @extern("LeaveCriticalSection");
extern fn Win32_BOOL tryEnterCriticalSection(Win32_CRITICAL_SECTION* section) @extern("TryEnterCriticalSection");
extern fn Win32_DWORD waitForSingleObject(Win32_HANDLE hHandle, Win32_DWORD dwMilliseconds) @extern("WaitForSingleObject");
extern fn Win32_DWORD waitForSingleObjectEx(Win32_HANDLE hHandle, Win32_DWORD dwMilliseconds, Win32_BOOL bAlertable) @extern("WaitForSingleObjectEx");
extern fn Win32_DWORD waitForMultipleObjects(Win32_DWORD nCount, Win32_HANDLE* lpHandles, Win32_BOOL bWaitAll, Win32_DWORD dwMilliseconds) @extern("WaitForMultipleObjects");
extern fn Win32_DWORD waitForMultipleObjectsEx(Win32_DWORD nCount, Win32_HANDLE* lpHandles, Win32_BOOL bWaitAll, Win32_DWORD dwMilliseconds, Win32_BOOL bAlertable) @extern("WaitForMultipleObjectsEx");
extern fn void sleep(uint ms) @extern("Sleep");
extern fn Win32_BOOL resetEvent(Win32_HANDLE event) @extern("ResetEvent");
extern fn Win32_BOOL setEvent(Win32_HANDLE handle) @extern("SetEvent");
extern fn long interlockedCompareExchange(int* dest, int exchange, int comperand) @extern("InterlockedCompareExchange");
extern fn Win32_DWORD sleepEx(Win32_DWORD ms, Win32_BOOL alertable) @extern("SleepEx");
extern fn Win32_HANDLE createThread(void* attributes, usz stack, ThreadFn func, Win32_LPVOID arg, Win32_DWORD flags, Win32_LPDWORD thread_id) @extern("CreateThread");
extern fn Win32_BOOL getExitCodeThread(Win32_HANDLE handle, Win32_LPDWORD exit_code) @extern("GetExitCodeThread");
extern fn Win32_BOOL getExitCodeProcess(Win32_HANDLE hProcess, Win32_LPDWORD lpExitCode) @extern("GetExitCodeProcess");
extern fn Win32_DWORD getThreadId(Win32_HANDLE) @extern("GetThreadId");
extern fn void exitThread(Win32_DWORD dwExitCode) @noreturn @extern("ExitThread");
extern fn Win32_HANDLE getCurrentThread() @extern("GetCurrentThread");
extern fn Win32_BOOL terminateProcess(Win32_HANDLE hProcess, Win32_UINT uExitCode) @extern("TerminateProcess");
extern fn Win32_DWORD getCurrentProcessId() @extern("GetCurrentProcessId");
extern fn Win32_HANDLE getCurrentProcess() @extern("GetCurrentProcess");
extern fn Win32_DWORD getCurrentThreadId() @extern("GetCurrentThreadId");
extern fn Win32_BOOL setHandleInformation(Win32_HANDLE hObject, Win32_DWORD dwMask, Win32_DWORD dwFlags) @extern("SetHandleInformation");
extern fn Win32_HANDLE createEventA(Win32_LPSECURITY_ATTRIBUTES lpEventAttributes, Win32_BOOL bManualReset, Win32_BOOL bInitialState, Win32_LPCSTR lpName) @extern("CreateEventA");
extern fn Win32_BOOL createProcessW(Win32_LPCWSTR lpApplicationName, Win32_LPWSTR lpCommandLine, Win32_LPSECURITY_ATTRIBUTES lpProcessAttributes, Win32_LPSECURITY_ATTRIBUTES lpThreadAttributes, Win32_BOOL bInheritHandles, Win32_DWORD dwCreationFlags, Win32_LPVOID lpEnvironment, Win32_LPCWSTR lpCurrentDirectory, Win32_LPSTARTUPINFOW lpStartupInfo, Win32_LPPROCESS_INFORMATION lpProcessInformation) @extern("CreateProcessW");
extern fn Win32_HANDLE createNamedPipeA(Win32_LPCSTR lpName, Win32_DWORD dwOpenMode, Win32_DWORD dwPipeMode, Win32_DWORD nMaxInstances, Win32_DWORD nOutBufferSize, Win32_DWORD nInBufferSize, Win32_DWORD nDefaultTimeOut, Win32_LPSECURITY_ATTRIBUTES lpSecurityAttributes) @extern("CreateNamedPipeA");
extern fn Win32_BOOL getOverlappedResult(Win32_HANDLE hFile, Win32_LPOVERLAPPED lpOverlapped, Win32_LPDWORD lpNumberOfBytesTransferred, Win32_BOOL bWait) @extern("GetOverlappedResult");
extern fn Win32_DWORD getEnvironmentVariableW(Win32_LPCWSTR lpName, Win32_LPWSTR  lpBuffer, Win32_DWORD nSize) @extern("GetEnvironmentVariableW");
extern fn Win32_BOOL setEnvironmentVariableW(Win32_LPCWSTR lpName, Win32_LPCWSTR lpValue) @extern("SetEnvironmentVariableW");
extern fn void getSystemInfo(Win32_LPSYSTEM_INFO lpSystemInfo) @extern("GetSystemInfo");
extern fn Win32_BOOL enumProcessModules(Win32_HANDLE hProcess, Win32_HMODULE* lphModule, Win32_DWORD cb, Win32_LPDWORD lpcbNeeded) @extern("K32EnumProcessModules");
extern fn Win32_BOOL getModuleInformation(Win32_HANDLE hProcess, Win32_HMODULE hModule, Win32_LPMODULEINFO lpmodinfo, Win32_DWORD cb) @extern("K32GetModuleInformation");
extern fn Win32_DWORD symAddrIncludeInlineTrace(Win32_HANDLE hProcess, Win32_DWORD64 address) @extern("SymAddrIncludeInlineTrace");
extern fn Win32_BOOL symQueryInlineTrace(Win32_HANDLE hProcess, Win32_DWORD64 startAddress, Win32_DWORD startContext, Win32_DWORD64 startRetAddress, Win32_DWORD64 curAddress, Win32_LPDWORD curContext, Win32_LPDWORD curFrameIndex) @extern("SymQueryInlineTrace");
extern fn Win32_BOOL symFromInlineContext(Win32_HANDLE hProcess, Win32_DWORD64 address, Win32_ULONG inlineContext, Win32_PDWORD64 displacement, Win32_PSYMBOL_INFO symbol) @extern("SymFromInlineContext");
extern fn Win32_BOOL symGetLineFromInlineContext(Win32_HANDLE hProcess, Win32_DWORD64 qwAddr, Win32_ULONG inlineContext, Win32_DWORD64 qwModuleBaseAddress, Win32_PDWORD pdwDisplacement, Win32_PIMAGEHLP_LINE64 line64) @extern("SymGetLineFromInlineContext");
extern fn Win32_ULONG rtlWalkFrameChain(Win32_PVOID*, Win32_ULONG, Win32_ULONG) @extern("RtlWalkFrameChain");
extern fn Win32_BOOL symInitialize(Win32_HANDLE hProcess, Win32_PCSTR userSearchPath, Win32_BOOL fInvadeProcess) @extern("SymInitialize");
extern fn Win32_BOOL symCleanup(Win32_HANDLE hProcess) @extern("SymCleanup");
extern fn Win32_DWORD getModuleFileNameA(Win32_HMODULE hModule, Win32_LPSTR lpFilename, Win32_DWORD nSize) @extern("GetModuleFileNameA");
extern fn Win32_DWORD getModuleFileNameExA(Win32_HANDLE hProcess, Win32_HMODULE hModule, Win32_LPSTR lpFilename, Win32_DWORD nSize) @extern("GetModuleFileNameExA");
extern fn Win32_DWORD64 symLoadModuleEx(Win32_HANDLE hProcess, Win32_HANDLE hFile, Win32_PCSTR imageName, Win32_PCSTR moduleName, Win32_DWORD64 baseOfDll, Win32_DWORD dllSize, Win32_PMODLOAD_DATA data, Win32_DWORD flags) @extern("SymLoadModule");
extern fn Win32_BOOL stackWalk64(Win32_DWORD machineType, Win32_HANDLE hProcess, Win32_HANDLE hThread, Win32_LPSTACKFRAME64 stackFrame, Win32_PVOID contextRecord, Win32_PREAD_PROCESS_MEMORY_ROUTINE64   readMemoryRoutine, Win32_PFUNCTION_TABLE_ACCESS_ROUTINE64 functionTableAccessRoutine, Win32_PGET_MODULE_BASE_ROUTINE64 getModuleBaseRoutine, Win32_PTRANSLATE_ADDRESS_ROUTINE64 translateAddress) @extern("StackWalk64");
extern fn void rtlCaptureContext(Win32_PCONTEXT contextRecord) @extern("RtlCaptureContext");
extern fn void* symFunctionTableAccess64(Win32_HANDLE  hProcess, Win32_DWORD64 addrBase) @extern("SymFunctionTableAccess64");
extern fn Win32_DWORD64 symGetModuleBase64(Win32_HANDLE  hProcess, Win32_DWORD64 qwAddr) @extern("SymGetModuleBase64");
extern fn Win32_DWORD getModuleBaseNameA(Win32_HANDLE  hProcess, Win32_HMODULE hModule, Win32_LPSTR lpBaseName, Win32_DWORD nSize) @extern("K32GetModuleBaseNameA");
extern fn Win32_DWORD symGetOptions() @extern("SymGetOptions");
extern fn Win32_DWORD symSetOptions(Win32_DWORD symOptions) @extern("SymSetOptions");
extern fn Win32_PIMAGE_NT_HEADERS imageNtHeader(Win32_PVOID base) @extern("ImageNtHeader");
extern fn Win32_DWORD unDecorateSymbolName(Win32_PCSTR name, Win32_PSTR outputString, Win32_DWORD maxStringLength, Win32_DWORD flags) @extern("UnDecorateSymbolName");
extern fn Win32_BOOL symFromAddr(Win32_HANDLE hProcess, Win32_DWORD64 address, Win32_PDWORD64 displacement, Win32_PSYMBOL_INFO symbol) @extern("SymFromAddr");
extern fn Win32_BOOL symGetLineFromAddr64(Win32_HANDLE hProcess, Win32_DWORD64 dwAddr, Win32_PDWORD pdwDisplacement, Win32_PIMAGEHLP_LINE64 line) @extern("SymGetLineFromAddr64");
extern fn Win32_WORD rtlCaptureStackBackTrace(Win32_DWORD framesToSkip, Win32_DWORD framesToCapture, Win32_PVOID *backTrace, Win32_PDWORD backTraceHash) @extern("RtlCaptureStackBackTrace");
extern fn Win32_BOOL symGetModuleInfo64(Win32_HANDLE hProcess, Win32_DWORD64 qwAddr, Win32_PIMAGEHLP_MODULE64 moduleInfo) @extern("SymGetModuleInfo64");
extern fn Win32_HANDLE getModuleHandleA(Win32_LPCSTR lpModuleName) @extern("GetModuleHandleA");
extern fn Win32_HANDLE getModuleHandleW(Win32_LPCWSTR lpModuleName) @extern("GetModuleHandleW");

fn Win32_DWORD! load_modules()
{
	Win32_HANDLE process = getCurrentProcess();
	Win32_DWORD needed;
	symInitialize(getCurrentProcess(), null, 1);
	Win32_DWORD symOptions = symGetOptions();
	symOptions |= SYMOPT_LOAD_LINES | SYMOPT_UNDNAME;
	symSetOptions(symOptions);
	Win32_HMODULE[1024] mod_buffer;
	if (!enumProcessModules(process, &mod_buffer, mod_buffer.len, &needed))
	{
		return BacktraceFault.RESOLUTION_FAILED?;
	}
	if (needed > mod_buffer.len) return BacktraceFault.RESOLUTION_FAILED?;
	Win32_HMODULE[] modules = mod_buffer[:needed];
	void* base = null;
	foreach (mod : modules)
	{
		Win32_MODULEINFO info;
		if (!getModuleInformation(process, mod, &info, $sizeof(info)))
		{
			return BacktraceFault.RESOLUTION_FAILED?;
		}
		if (!base) base = info.lpBaseOfDll;
		Win32_DWORD load_size = info.sizeOfImage;
		char[1024] char_buf;
		Win32_DWORD len = getModuleFileNameA(mod, (Win32_LPSTR)&char_buf, char_buf.len - 1);
		if (len < 1) continue;
		char[1024] module_name;
		Win32_DWORD len2 = getModuleBaseNameA(process, mod, (Win32_LPSTR)&module_name, 1021);
		if (len2 < 1) continue;
		Win32_DWORD64 base_addr = symLoadModuleEx(process, null, (Win32_PCSTR)&char_buf, (Win32_PCSTR)&module_name, (Win32_DWORD64)info.lpBaseOfDll, load_size, null, 0);
	}
	if (!base) return BacktraceFault.IMAGE_NOT_FOUND?;
	Win32_IMAGE_NT_HEADERS* h = imageNtHeader(base);
	return h.fileHeader.machine;
}

struct Symbol
{
	inline Win32_SYMBOL_INFO sym;
	char[256] buffer;
}

Win32_DWORD64 displacement;

fn BacktraceList! symbolize_backtrace(void*[] backtrace, Allocator allocator)
{
	BacktraceList list;
	list.new_init(backtrace.len, allocator);
	Win32_HANDLE process = getCurrentProcess();
	symInitialize(process, null, 1);
	defer symCleanup(process);
	foreach (addr : backtrace)
	{
		list.push(resolve_backtrace(addr, process, allocator) ?? backtrace::BACKTRACE_UNKNOWN);
	}
	return list;
}

fn Backtrace! resolve_backtrace(void* addr, Win32_HANDLE process, Allocator allocator)
{
	Symbol symbol;
	//Win32_DWORD image_type = load_modules()!;
	symbol.sizeOfStruct = Win32_SYMBOL_INFO.sizeof;
	symbol.maxNameLen = 255;
	if (!symFromAddr(process, (Win32_DWORD64)addr - 1, &displacement, &symbol))
	{
		return BacktraceFault.NO_BACKTRACE_SYMBOLS?;
	}
	Win32_IMAGEHLP_MODULE64 module_info;
	module_info.sizeOfStruct = Win32_IMAGEHLP_MODULE64.sizeof;
	if (!symGetModuleInfo64(process, (Win32_DWORD64)addr - 1, &module_info))
	{
		return BacktraceFault.NO_BACKTRACE_SYMBOLS?;
	}
	ZString module_name = (ZString)&module_info.imageName;
	char[256] name;
	unDecorateSymbolName(&symbol.name, (Win32_PSTR)&name, 256, UNDNAME_COMPLETE);
	Win32_DWORD offset = 0;
	Win32_IMAGEHLP_LINE64 line;
	Backtrace backtrace;
	ZString zname = (ZString)&name;
	if (!symGetLineFromAddr64(process, (Win32_ULONG64)addr - 1, &offset, &line))
	{
		backtrace.init((uptr)addr, zname.str_view(), module_name.str_view(), allocator: allocator);
		return backtrace;
	}
	String filename = ((ZString)line.fileName).str_view();
	backtrace.init((uptr)addr, zname.str_view(), module_name.str_view(), file: filename, line: line.lineNumber, allocator: allocator);
	return backtrace;
}
