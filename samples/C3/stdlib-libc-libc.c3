// Source: https://github.com/c3lang/c3c/blob/master/lib/std/libc/libc.c3
// Permalink: https://github.com/c3lang/c3c/blob/1b54a99/lib/std/libc/libc.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

// Copyright (c) 2021 Christoffer Lerno. All rights reserved.
// Use of this source code is governed by the MIT license
// a copy of which can be found in the LICENSE_STDLIB file.
module libc;

// Constants need to be per os/arch
const int EXIT_FAILURE = 1;
const int EXIT_SUCCESS = 0;
const int RAND_MAX = 0x7fffffff;

struct DivResult
{
	CInt quot;
	CInt rem;
}

struct LongDivResult
{
	CLong quot;
	CLong rem;
}

fn Errno errno()
{
	return (Errno)os::errno();
}

fn void errno_set(Errno e)
{
	os::errno_set((int)e);
}

distinct Errno = inline CInt;
def TerminateFunction = fn void();
def CompareFunction = fn int(void*, void*);
def JmpBuf = uptr[$$JMP_BUF_SIZE];
def Fd = CInt;
def Fpos_t = long; // TODO make sure fpos is correct on all targets.
def SignalFunction = fn void(CInt);

const CInt SIGHUP = 1;
const CInt SIGINT = 2;
const CInt SIGQUIT = 3;
const CInt SIGILL = 4;
const CInt SIGTRAP = 5;
const CInt SIGABTR = 6;
const CInt SIGBUS = BSD_FLAVOR_SIG ? 10 : 7; // Or Mips
const CInt SIGFPE = 8;
const CInt SIGKILL = 9;
const CInt SIGSEGV = 11;
const CInt SIGSYS = BSD_FLAVOR_SIG ? 12 : 31;
const CInt SIGPIPE = 13;
const CInt SIGALRM = 14;
const CInt SIGTERM = 15;
const CInt SIGURG = BSD_FLAVOR_SIG ? 16 : 23;
const CInt SIGSTOP = BSD_FLAVOR_SIG ? 17 : 19;
const CInt SIGTSTP = BSD_FLAVOR_SIG ? 18 : 20;
const CInt SIGCONT = BSD_FLAVOR_SIG ? 19 : 18;
const CInt SIGCHLD = BSD_FLAVOR_SIG ? 20 : 17;

const bool BSD_FLAVOR_SIG @local = env::OPENBSD || env::DARWIN || env::FREEBSD || env::NETBSD;

def Time_t = $typefrom(env::WIN32 ? long.typeid : CLong.typeid);
def Off_t = $typefrom(env::WIN32 ? int.typeid : usz.typeid);

struct Timespec
{
	Time_t tv_sec;
	CLong tv_nsec;
}

module libc @if(env::LIBC);

extern fn void abort();
extern fn CInt abs(CInt n);
extern fn ZString asctime(Tm* timeptr);
extern fn ZString asctime_r(Tm* timeptr, char* buf);
extern fn CInt atexit(TerminateFunction func);
extern fn double atof(char* str);
extern fn int atoi(char* str);
extern fn CLongLong atoll(char* str);
extern fn void bsearch(void* key, void *base, usz items, usz size, CompareFunction compare);
extern fn void* calloc(usz count, usz size);
extern fn void clearerr(CFile stream);
extern fn Clock_t clock();
extern fn CInt close(CInt fd) @if(!env::WIN32);
extern fn double difftime(Time_t time1, Time_t time2) @if(!env::WIN32);
extern fn DivResult div(CInt numer, CInt denom);
extern fn void exit(CInt status);
extern fn CInt fclose(CFile stream);
extern fn CFile fdopen(CInt fd, ZString mode) @if(!env::WIN32);
extern fn CInt feof(CFile stream);
extern fn CInt ferror(CFile stream);
extern fn CInt fflush(CFile stream);
extern fn CInt fgetc(CFile stream);
extern fn ZString fgets(char* string, CInt n, CFile stream);
extern fn CInt fgetpos(CFile stream, Fpos_t* pos);
extern fn Fd fileno(CFile stream) @if(!env::WIN32);
extern fn CFile fopen(ZString filename, ZString mode);
extern fn CInt fprintf(CFile stream, ZString format, ...);
extern fn CInt fputc(CInt c, CFile stream);
extern fn CInt fputs(ZString string, CFile stream);
extern fn usz fread(void* ptr, usz size, usz nmemb, CFile stream);
extern fn void* free(void*);
extern fn CFile freopen(ZString filename, ZString mode, CFile stream);
extern fn CInt fscanf(CFile stream, ZString format, ...);
extern fn CInt fseek(CFile stream, SeekIndex offset, CInt whence) @if(!env::WIN32);
extern fn CInt fsetpos(CFile stream, Fpos_t* pos);
extern fn SeekIndex ftell(CFile stream) @if(!env::WIN32);
extern fn usz fwrite(void* ptr, usz size, usz nmemb, CFile stream);
extern fn CInt getc(CFile stream);
extern fn CInt getchar();
extern fn ZString getenv(ZString name);
extern fn ZString gets(char* buffer);
extern fn Tm* gmtime(Time_t* timer);
extern fn Tm* gmtime_r(Time_t *timer, Tm* buf) @if(!env::WIN32);
extern fn CLong labs(CLong x);
extern fn LongDivResult ldiv(CLong number, CLong denom);
extern fn Tm* localtime(Time_t* timer);
extern fn Tm* localtime_r(Time_t* timer, Tm* result) @if(!env::WIN32);
extern fn void longjmp(JmpBuf* buffer, CInt value);
extern fn void* malloc(usz size);
extern fn void* memchr(void* str, CInt c, usz n);
extern fn CInt memcmp(void* buf1, void* buf2, usz count);
extern fn void* memcpy(void* dest, void* src, usz n);
extern fn void* memmove(void* dest, void* src, usz n);
extern fn void* memset(void* dest, CInt value, usz n);
extern fn Time_t* mktime(Tm* time) @if(!env::WIN32);
extern fn void perror(ZString string);
extern fn CInt printf(ZString format, ...);
extern fn CInt putc(CInt c, CFile stream);
extern fn CInt putchar(CInt c);
extern fn CInt puts(ZString str);
extern fn void qsort(void* base, usz items, usz size, CompareFunction compare);
extern fn CInt raise(CInt signal);
extern fn CInt rand();
extern fn isz read(Fd fd, void* buf, usz nbyte) @if(!env::WIN32);
extern fn void* realloc(void* ptr, usz size);
extern fn CInt remove(ZString filename);
extern fn CInt rename(ZString old_name, ZString new_name);
extern fn void rewind(CFile stream);
extern fn CInt scanf(ZString format, ...);
extern fn void setbuf(CFile stream, char* buffer);
extern fn int setenv(ZString name, ZString value, CInt overwrite);
extern fn CInt setjmp(JmpBuf* buffer) @if(!env::WIN32);
extern fn void setvbuf(CFile stream, char* buf, CInt type, usz size);
extern fn SignalFunction signal(CInt sig, SignalFunction function);
extern fn CInt snprintf(char* buffer, usz size, ZString format, ...);
extern fn CInt sprintf(char* buffer, ZString format, ...);
extern fn void srand(uint seed);
extern fn CInt sscanf(char* buffer, ZString format, ...);
extern fn ZString strcat(ZString dest, ZString src);
extern fn char* strchr(char* str, CInt c);
extern fn CInt strcmp(ZString str1, ZString str2);
extern fn CInt strcoll(ZString str1, ZString str2);
extern fn usz strcspn(ZString str1, ZString str2);
extern fn ZString strcpy(ZString dst, ZString src);
extern fn ZString strerror(CInt errn);
extern fn usz strftime(char* dest, usz maxsize, ZString format, Tm* timeptr);
extern fn usz strlen(ZString str);
extern fn ZString strncat(char* dest, char* src, usz n);
extern fn CInt strncmp(char* str1, char* str2, usz n);
extern fn char* strncpy(char* dst, char* src, usz n);
extern fn CULong stroul(char* str, char** endptr, int base);
extern fn char* strpbrk(ZString str1, ZString str2);
extern fn usz strspn(ZString str1, ZString str2);
extern fn ZString strptime(char* buf, ZString format, Tm* tm);
extern fn char* strrchr(ZString str, CInt c);
extern fn char* strstr(ZString haystack, ZString needle);
extern fn double strtod(char* str, char** endptr);
extern fn float strtof(char* str, char** endptr);
extern fn ZString strtok(ZString str, ZString delim);
extern fn CLong strtol(char* str, char** endptr, CInt base);
extern fn CULong strtul(char* str, char** endptr, CInt base);
extern fn usz strxfrm(char* dest, ZString src, usz n);
extern fn CInt system(ZString str);
extern fn Time_t timegm(Tm *timeptr) @if(!env::WIN32);
extern fn ZString tmpnam(ZString str);
extern fn CInt ungetc(CInt c, CFile stream);
extern fn CInt unsetenv(ZString name);
extern fn isz write(Fd fd, void* buffer, usz count) @if(!env::WIN32);

extern fn CFile fmemopen(void* ptr, usz size, ZString mode);
extern fn isz getline(char** linep, usz* linecapp, CFile stream);
extern fn CInt timespec_get(TimeSpec* ts, CInt base);
extern fn CInt nanosleep(TimeSpec* req, TimeSpec* remaining);
extern fn ZString ctime(Time_t *timer);
extern fn Time_t time(Time_t *timer);

const CInt STDIN_FD = 0;
const CInt STDOUT_FD = 1;
const CInt STDERR_FD = 2;

module libc @if(env::LINUX);
extern CFile __stdin @extern("stdin");
extern CFile __stdout @extern("stdout");
extern CFile __stderr @extern("stderr");
extern fn usz malloc_usable_size(void* ptr);
macro usz malloc_size(void* ptr) => malloc_usable_size(ptr);
extern fn void* aligned_alloc(usz align, usz size);
macro CFile stdin() => __stdin;
macro CFile stdout() => __stdout;
macro CFile stderr() => __stderr;

module libc @if(env::DARWIN || env::BSD_FAMILY);
extern CFile __stdinp;
extern CFile __stdoutp;
extern CFile __stderrp;
extern fn usz malloc_size(void* ptr);
extern fn void* aligned_alloc(usz align, usz size);
macro CFile stdin() => __stdinp;
macro CFile stdout() => __stdoutp;
macro CFile stderr() => __stderrp;

module libc @if(env::WIN32);
macro usz malloc_size(void* ptr) => _msize(ptr);
macro CFile stdin() => __acrt_iob_func(STDIN_FD);
macro CFile stdout() => __acrt_iob_func(STDOUT_FD);
macro CFile stderr() => __acrt_iob_func(STDERR_FD);

module libc @if(env::LIBC && !env::WIN32 && !env::LINUX && !env::DARWIN && !env::BSD_FAMILY);
macro CFile stdin() { return (CFile*)(uptr)STDIN_FD; }
macro CFile stdout() { return (CFile*)(uptr)STDOUT_FD; }
macro CFile stderr() { return (CFile*)(uptr)STDERR_FD; }

module libc @if(!env::LIBC);

fn void longjmp(JmpBuf* buffer, CInt value) @weak @extern("longjmp") @nostrip
{
	unreachable("longjmp unavailable");
}

fn CInt setjmp(JmpBuf* buffer) @weak @extern("setjmp") @nostrip
{
	unreachable("setjmp unavailable");
}

fn void* malloc(usz size) @weak @extern("malloc") @nostrip
{
	unreachable("malloc unavailable");
}
fn void* calloc(usz count, usz size) @weak @extern("calloc") @nostrip
{
	unreachable("calloc unavailable");
}
fn void* free(void*) @weak @extern("free")
{
	unreachable("free unavailable");
}

fn void* realloc(void* ptr, usz size) @weak @extern("realloc") @nostrip
{
	unreachable("realloc unavailable");
}

fn void* memcpy(void* dest, void* src, usz n) @weak @extern("memcpy") @nostrip
{
	for (usz i = 0; i < n; i++) ((char*)dest)[i] = ((char*)src)[i];
	return dest;
}

fn void* memmove(void* dest, void* src, usz n) @weak @extern("memmove") @nostrip
{
	return memcpy(dest, src, n) @inline;
}

fn void* memset(void* dest, CInt value, usz n) @weak @extern("memset") @nostrip
{
	for (usz i = 0; i < n; i++) ((char*)dest)[i] = (char)value;
	return dest;
}

fn int fseek(CFile stream, SeekIndex offset, int whence) @weak @extern("fseek") @nostrip
{
	unreachable("'fseek' not available.");
}
fn CFile fopen(ZString filename, ZString mode) @weak @extern("fopen") @nostrip
{
	unreachable("'fopen' not available.");
}

fn CFile freopen(ZString filename, ZString mode, CFile stream) @weak @extern("fopen") @nostrip
{
	unreachable("'freopen' not available.");
}

fn usz fwrite(void* ptr, usz size, usz nmemb, CFile stream) @weak @extern("fwrite") @nostrip
{
	unreachable("'fwrite' not available.");
}

fn usz fread(void* ptr, usz size, usz nmemb, CFile stream) @weak @extern("fread") @nostrip
{
	unreachable("'fread' not available.");
}

fn CFile fclose(CFile) @weak @extern("fclose") @nostrip
{
	unreachable("'fclose' not available.");
}

fn int fflush(CFile stream) @weak @extern("fflush") @nostrip
{
	unreachable("'fflush' not available.");
}

fn int fputc(int c, CFile stream) @weak @extern("fputc") @nostrip
{
	unreachable("'fputc' not available.");
}

fn char* fgets(ZString str, int n, CFile stream) @weak @extern("fgets") @nostrip
{
	unreachable("'fgets' not available.");
}

fn int fgetc(CFile stream) @weak @extern("fgetc") @nostrip
{
	unreachable("'fgetc' not available.");
}

fn int feof(CFile stream) @weak @extern("feof") @nostrip
{
	unreachable("'feof' not available.");
}

fn int putc(int c, CFile stream) @weak @extern("putc") @nostrip
{
	unreachable("'putc' not available.");
}
fn int putchar(int c) @weak @extern("putchar") @nostrip
{
	unreachable("'putchar' not available.");
}
fn int puts(ZString str) @weak @extern("puts") @nostrip
{
	unreachable("'puts' not available.");
}

module libc;

// stdio

def CFile = void*;


const HAS_MALLOC_SIZE = env::LINUX || env::WIN32 || env::DARWIN;

// The following needs to be set per arch+os
// For now I have simply pulled the defaults from MacOS
const int SEEK_SET = 0;
const int SEEK_CUR = 1;
const int SEEK_END = 2;
const int _IOFBF = 0; // Fully buffered
const int _IOLBF = 1; // Line buffered
const int _IONBF = 2; // Unbuffered
const int BUFSIZ = 1024;
const int EOF = -1;
const int FOPEN_MAX = 20;
const int FILENAME_MAX = 1024;

macro bool libc_S_ISTYPE(value, mask) @builtin => (value & S_IFMT) == mask;
const S_IFMT   = 0o170000; // type of file mask
const S_IFIFO  = 0o010000; // named pipe (fifo)
const S_IFCHR  = 0o020000; // character special
const S_IFDIR  = 0o040000; // directory
const S_IFBLK  = 0o060000; // block special
const S_IFREG  = 0o100000; // regular
const S_IFLNK  = 0o120000; // symbolic link
const S_IFSOCK = 0o140000; // socket
const S_ISUID  = 0o004000; // Set user id on execution
const S_ISGID  = 0o002000; // Set group id on execution
const S_ISVTX  = 0o001000; // Save swapped text even after use
const S_IRUSR  = 0o000400; // Read permission, owner
const S_IWUSR  = 0o000200; // Write permission, owner
const S_IXUSR  = 0o000100; // Execute/search permission, owner

def SeekIndex = CLong;

// vsprintf vprintf not supported

// time.h

struct Tm
{
	CInt	tm_sec;		// seconds after the minute [0-60]
	CInt	tm_min;		// minutes after the hour [0-59]
	CInt	tm_hour;	// hours since midnight [0-23]
	CInt	tm_mday;	// day of the month [1-31]
	CInt	tm_mon;		// months since January [0-11]
	CInt	tm_year;	// years since 1900
	CInt	tm_wday;	// days since Sunday [0-6]
	CInt	tm_yday;	// days since January 1 [0-365]
	CInt	tm_isdst;	// Daylight Savings Time flag
	TimeOffset tm_gmtoff @if(!env::WIN32);	/* offset from UTC in seconds */
	char *tm_zone @if(!env::WIN32);	/* timezone abbreviation */
	CInt tm_nsec @if(env::WASI);
}

struct TimeSpec
{
	Time_t s;
	ulong ns @if(env::WIN32);
	CLong ns @if(!env::WIN32);
}


def Clock_t = int @if(env::WIN32);
def Clock_t = CULong @if(!env::WIN32);

def TimeOffset = int @if(env::WASI) ;
def TimeOffset = CLong @if(!env::WASI) ;

const int TIME_UTC = 1;


// Likely wrong, must be per platform.
const CLOCKS_PER_SEC = 1000000;

module libc::errno;

const Errno OK                       = 0;
const Errno EPERM                    = 1; // Operation not permitted
const Errno ENOENT                   = 2; // No such file or directory
const Errno ESRCH                    = 3; // No such process
const Errno EINTR                    = 4; // Interrupted system call
const Errno EIO                      = 5; // I/O error
const Errno ENXIO                    = 6; // No such device or address
const Errno E2BIG                    = 7; // Argument list too long
const Errno ENOEXEC                  = 8; // Exec format error
const Errno EBADF                    = 9; // Bad file number
const Errno ECHILD                   = 10; // No child processes


const Errno EAGAIN @if(env::DARWIN)  = 35; // Try again Macos
const Errno EAGAIN @if(!env::DARWIN) = 11; // Try again

const Errno ENOMEM                   = 12; // Out of memory
const Errno EACCES                   = 13; // Permission denied
const Errno EFAULT                   = 14; // Bad address
const Errno ENOTBLK                  = 15; // Block device required, not on Win32
const Errno EBUSY                    = 16; // Device or resource busy
const Errno EEXIST                   = 17; // File exists
const Errno EXDEV                    = 18; // Cross-device link
const Errno ENODEV                   = 19; // No such device
const Errno ENOTDIR                  = 20; // Not a directory
const Errno EISDIR                   = 21; // Is a directory
const Errno EINVAL                   = 22; // Invalid argument
const Errno ENFILE                   = 23; // File table overflow
const Errno EMFILE                   = 24; // Too many open files
const Errno ENOTTY                   = 25; // Not a typewriter
const Errno ETXTBSY                  = 26; // Text file busy, not on Win32
const Errno EFBIG                    = 27; // File too large
const Errno ENOSPC                   = 28; // No space left on device
const Errno ESPIPE                   = 29; // Illegal seek
const Errno EROFS                    = 30; // Read-only file system
const Errno EMLINK                   = 31; // Too many links
const Errno EPIPE                    = 32; // Broken pipe
const Errno EDOM                     = 33; // Math argument out of domain of func
const Errno ERANGE                   = 34; // Math result not representable

// https://developer.apple.com/library/archive/documentation/System/Conceptual/ManPages_iPhoneOS/man2/intro.2.html
module libc::errno @if(env::DARWIN);
const Errno EWOULDBLOCK              = EAGAIN; // Operation would block
const Errno EDEADLK                  = 11; // Resource deadlock would occur
const Errno EINPROGRESS              = 36; // Operation now in progress
const Errno EALREADY                 = 37; // Operation already in progress
const Errno ENOTSOCK                 = 38; // Socket operation on non-socket
const Errno EDESTADDRREQ             = 39; // Destination address required
const Errno EMSGSIZE                 = 40; // Message too long
const Errno EPROTOTYPE               = 41; // Protocol wrong type for socket
const Errno ENOPROTOOPT              = 42; // Protocol not available
const Errno EPROTONOSUPPORT          = 43; // Protocol not supported
const Errno ESOCKTNOSUPPORT          = 44; // Socket type not supported
const Errno ENOTSUP                  = 45; // Not supported
const Errno EPFNOSUPPORT             = 46; // Protocol family not supported
const Errno EAFNOSUPPORT             = 47; // Address family not supported by protocol family
const Errno EADDRINUSE               = 48; // Address already in use
const Errno EADDRNOTAVAIL            = 49; // Cannot assign requested address
const Errno ENETDOWN                 = 50; // Network is down
const Errno ENETUNREACH              = 51; // Network is unreachable
const Errno ENETRESET                = 52; // Network dropped connection on reset
const Errno ECONNABORTED             = 53; // Software caused connection abort
const Errno ECONNRESET               = 54; // Connection reset by peer
const Errno ENOBUFS                  = 55; // No buffer space available
const Errno EISCONN                  = 56; // Socket is already connected
const Errno ENOTCONN                 = 57; // Socket is not connected
const Errno ESHUTDOWN                = 58; // Cannot send after socket shutdown
const Errno ETIMEDOUT                = 60; // Operation timed out
const Errno ECONNREFUSED             = 61; // Connection refused
const Errno ELOOP                    = 62; // Too many levels of symbolic links
const Errno ENAMETOOLONG             = 63; // File name too long
const Errno EHOSTDOWN                = 64; // Host is down
const Errno EHOSTUNREACH             = 65; // No route to host
const Errno ENOTEMPTY                = 66; // Directory not empty
const Errno EPROCLIM                 = 67; // Too many processes
const Errno EUSERS                   = 68; // Too many users
const Errno EDQUOT                   = 69; // Disc quota exceeded
const Errno ESTALE                   = 70; // Stale NFS file handle
const Errno EBADRPC                  = 72; // RPC struct is bad
const Errno ERPCMISMATCH             = 73; // RPC version wrong
const Errno EPROGUNAVAIL             = 74; // RPC prog. not avail
const Errno EPROGMISMATCH            = 75; // Program version wrong
const Errno EPROCUNAVAIL             = 76; // Bad procedure for program
const Errno ENOLCK                   = 77; // No locks available
const Errno ENOSYS                   = 78; // Function not implemented
const Errno EFTYPE                   = 79; // Inappropriate file type or format
const Errno EAUTH                    = 80; // Authentication error
const Errno ENEEDAUTH                = 81; // Need authenticator
const Errno EPWROFF                  = 82; // Device power is off
const Errno EDEVERR                  = 83; // Device error
const Errno EOVERFLOW                = 84; // Value too large to be stored in data type
const Errno EBADEXEC                 = 85; // Bad executable (or shared library)
const Errno EBADARCH                 = 86; // Bad CPU type in executable
const Errno ESHLIBVERS               = 87; // Shared library version mismatch
const Errno EBADMACHO                = 88; // Malformed Mach-o file
const Errno ECANCELED                = 89; // Operation canceled
const Errno EIDRM                    = 90; // Identifier removed
const Errno ENOMSG                   = 91; // No message of desired type
const Errno EILSEQ                   = 92; // Illegal byte sequence
const Errno ENOATTR                  = 93; // Attribute not found
const Errno EBADMSG                  = 94; // Bad message
const Errno EMULTIHOP                = 95; // Reserved
const Errno ENODATA                  = 96; // No message available
const Errno ENOLINK                  = 97; // Reserved
const Errno ENOSR                    = 98; // No STREAM resources
const Errno ENOSTR                   = 99; // Not a STREAM
const Errno EPROTO                   = 100; // Protocol error
const Errno ETIME                    = 101; // STREAM ioctl() timeout
const Errno EOPNOTSUPP               = 102; // Operation not supported on socket

module libc::errno @if(env::WIN32);
const Errno EDEADLK                  = 36; // Resource deadlock would occur Win32
const Errno ENAMETOOLONG             = 38; // File name too long Win32
const Errno ENOTEMPTY                = 41; // Directory not empty
const Errno ELOOP                    = 114; // Too many symbolic links encountered
const Errno EOVERFLOW                = 132; // Value too large for defined data type
const Errno ENETDOWN                 = 116; // Network is down
const Errno ECONNRESET               = 108; // Connection reset by peer
const Errno ENETUNREACH              = 118; // Network is unreachable
const Errno ENETRESET                = 117; // Network dropped connection because of reset
const Errno EOPNOTSUPP               = 130; // Operation not supported on transport endpoint
const Errno ETIMEDOUT                = 138; // Connection timed out
const Errno EALREADY                 = 103; // Operation already in progress
const Errno EINPROGRESS              = 112; // Operation now in progress Win32
const Errno EDQUOT                   = -122; // Quota exceeded, not in Win32
const Errno EWOULDBLOCK              = 140; // Operation would block

module libc::errno @if(!env::WIN32 && !env::DARWIN);
const Errno EDEADLK                  = 35; // Resource deadlock would occur Linux (others?)
const Errno ENAMETOOLONG             = 36; // File name too long Linux (others?)
const Errno ENOTEMPTY                = 39; // Directory not empty
const Errno ELOOP                    = 40; // Too many symbolic links encountered
const Errno EWOULDBLOCK              = EAGAIN; // Operation would block
const Errno EOVERFLOW                = 75; // Value too large for defined data type
const Errno ENOTSOCK                 = 88; // Socket operation on non-socket
const Errno EOPNOTSUPP               = 95; // Operation not supported on transport endpoint
const Errno EADDRINUSE               = 98; // Address already in use
const Errno EADDRNOTAVAIL            = 99; // Cannot assign requested address
const Errno ENETDOWN                 = 100; // Network is down
const Errno ENETUNREACH              = 101; // Network is unreachable
const Errno ENETRESET                = 102; // Network dropped connection because of reset
const Errno ECONNRESET               = 104; // Connection reset by peer
const Errno EISCONN                  = 106; // Socket is already connected
const Errno ETIMEDOUT                = 110; // Connection timed out
const Errno ECONNREFUSED             = 111; // Connection refused
const Errno EALREADY                 = 114; // Operation already in progress
const Errno EINPROGRESS              = 115; // Operation now in progress
const Errno EDQUOT                   = 122; // Quota exceeded


/*
const Errno ENOLCK = 37;  /* No record locks available */
const Errno ENOSYS = 38;  /* Function not implemented */

const Errno ENOMSG = 42;  /* No message of desired type */
const Errno EIDRM = 43;  /* Identifier removed */
const Errno ECHRNG = 44;  /* Channel number out of range */
const Errno EL2NSYNC = 45;  /* Level 2 not synchronized */
const Errno EL3HLT = 46;  /* Level 3 halted */
const Errno EL3RST = 47;  /* Level 3 reset */
const Errno ELNRNG = 48;  /* Link number out of range */
const Errno EUNATCH = 49;  /* Protocol driver not attached */
const Errno ENOCSI = 50;  /* No CSI structure available */
const Errno EL2HLT = 51;  /* Level 2 halted */
const Errno EBADE = 52;  /* Invalid exchange */
const Errno EBADR = 53;  /* Invalid request descriptor */
const Errno EXFULL = 54;  /* Exchange full */
const Errno ENOANO = 55;  /* No anode */
const Errno EBADRQC = 56;  /* Invalid request code */
const Errno EBADSLT = 57;  /* Invalid slot */

const Errno EBFONT = 59;  /* Bad font file format */
const Errno ENOSTR = 60;  /* Device not a stream */
const Errno ENODATA = 61;  /* No data available */
const Errno ETIME = 62;  /* Timer expired */
const Errno ENOSR = 63;  /* Out of streams resources */
const Errno ENONET = 64;  /* Machine is not on the network */
const Errno ENOPKG = 65;  /* Package not installed */
const Errno EREMOTE = 66;  /* Object is remote */
const Errno ENOLINK = 67;  /* Link has been severed */
const Errno EADV = 68;  /* Advertise error */
const Errno ESRMNT = 69;  /* Srmount error */
const Errno ECOMM = 70;  /* Communication error on send */
const Errno EPROTO = 71;  /* Protocol error */
const Errno EMULTIHOP = 72;  /* Multihop attempted */
const Errno EDOTDOT = 73;  /* RFS specific error */
const Errno EBADMSG = 74;  /* Not a data message */
const Errno ENOTUNIQ = 76;  /* Name not unique on network */
const Errno EBADFD = 77;  /* File descriptor in bad state */
const Errno EREMCHG = 78;  /* Remote address changed */
const Errno ELIBACC = 79;  /* Can not access a needed shared library */
const Errno ELIBBAD = 80;  /* Accessing a corrupted shared library */
const Errno ELIBSCN = 81;  /* .lib section in a.out corrupted */
const Errno ELIBMAX = 82;  /* Attempting to link in too many shared libraries */
const Errno ELIBEXEC = 83;  /* Cannot exec a shared library directly */
const Errno EILSEQ = 84;  /* Illegal byte sequence */
const Errno ERESTART = 85;  /* Interrupted system call should be restarted */
const Errno ESTRPIPE = 86;  /* Streams pipe error */
const Errno EUSERS = 87;  /* Too many users */
const Errno ENOTSOCK = 88;  /* Socket operation on non-socket */
const Errno EDESTADDRREQ = 89;  /* Destination address required */
const Errno EMSGSIZE = 90;  /* Message too long */
const Errno EPROTOTYPE = 91;  /* Protocol wrong type for socket */
const Errno ENOPROTOOPT = 92;  /* Protocol not available */
const Errno EPROTONOSUPPORT = 93;  /* Protocol not supported */
const Errno ESOCKTNOSUPPORT = 94;  /* Socket type not supported */
const Errno EPFNOSUPPORT = 96;  /* Protocol family not supported */
const Errno EAFNOSUPPORT = 97;  /* Address family not supported by protocol */
const Errno EADDRINUSE = 98;  /* Address already in use */
const Errno ECONNABORTED = 103; /* Software caused connection abort */
const Errno ENOBUFS = 105; /* No buffer space available */
const Errno EISCONN = 106; /* Transport endpoint is already connected */
const Errno ENOTCONN = 107; /* Transport endpoint is not connected */
const Errno ESHUTDOWN = 108; /* Cannot send after transport endpoint shutdown */
const Errno ETOOMANYREFS = 109; /* Too many references: cannot splice */
const Errno ECONNREFUSED = 111; /* Connection refused */
const Errno EHOSTDOWN = 112; /* Host is down */
const Errno EHOSTUNREACH = 113; /* No route to host */
*/


/*
const Errno ESTALE = 116; /* Stale NFS file handle */
const Errno EUCLEAN = 117; /* Structure needs cleaning */
const Errno ENOTNAM = 118; /* Not a XENIX named type file */
const Errno ENAVAIL = 119; /* No XENIX semaphores available */
const Errno EISNAM = 120; /* Is a named type file */
const Errno EREMOTEIO = 121; /* Remote I/O error */

const Errno ENOMEDIUM = 123; /* No medium found */
const Errno EMEDIUMTYPE = 124; /* Wrong medium type */
const Errno ECANCELED = 125; /* Operation Canceled */
const Errno ENOKEY = 126; /* Required key not available */
const Errno EKEYEXPIRED = 127; /* Key has expired */
const Errno EKEYREVOKED = 128; /* Key has been revoked */
const Errno EKEYREJECTED = 129; /* Key was rejected by service */

const Errno EOWNERDEAD = 130; /* Owner died */
const Errno ENOTRECOVERABLE = 131; /* State not recoverable */
*/

