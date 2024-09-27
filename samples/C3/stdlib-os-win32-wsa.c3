// Source: https://github.com/c3lang/c3c/blob/master/lib/std/os/win32/wsa.c3
// Permalink: https://github.com/c3lang/c3c/blob/b7381fc/lib/std/os/win32/wsa.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::os::win32 @if(env::WIN32);

// See https://github.com/wine-mirror/wine/blob/master/include/winsock2.h

distinct WSAError = int;
struct Win32_pollfd
{
	Win32_SOCKET fd;
	Win32_SHORT events;
	Win32_SHORT revents;
}
def Win32_WSAPOLLFD = Win32_pollfd;
def Win32_PWSAPOLLFD = Win32_WSAPOLLFD*;
def Win32_LPWSAPOLLFD = Win32_WSAPOLLFD*;

const Win32_SHORT POLLERR    = 0x0001;
const Win32_SHORT POLLHUP    = 0x0002;
const Win32_SHORT POLLNVAL   = 0x0004;
const Win32_SHORT POLLWRNORM = 0x0010;
const Win32_SHORT POLLWRBAND = 0x0020;
const Win32_SHORT POLLRDNORM = 0x0100;
const Win32_SHORT POLLRDBAND = 0x0200;
const Win32_SHORT POLLPRI    = 0x0400;
const Win32_SHORT POLLIN     = POLLRDNORM | POLLRDBAND;
const Win32_SHORT POLLOUT    = POLLWRNORM;

const SD_RECEIVE = 0x00;
const SD_SEND    = 0x01;
const SD_BOTH    = 0x02;

extern fn CInt win32_WSAPoll(Win32_LPWSAPOLLFD fdArray, Win32_ULONG fds, Win32_INT timeout) @extern("WSAPoll") @builtin;
extern fn WSAError win32_WSAGetLastError() @extern("WSAGetLastError") @builtin;
extern fn void win32_WSASetLastError(WSAError error) @extern("WSASetLastError") @builtin;
extern fn CInt win32_WSAStartup(Win32_WORD, void*) @extern("WSAStartup") @builtin;
extern fn CInt win32_WSACleanup() @extern("WSACleanup") @builtin;

const int FIONBIO = -2147195266;
const int FIONREAD = 1074030207;
const int SIOCATMARK = 1074033415;

module std::os::win32::wsa @if(env::WIN32);

const WSAError NO_ERROR                     = 0;
const WSAError INVALID_HANDLE               = 6;
const WSAError NOT_ENOUGH_MEMORY            = 8;
const WSAError INVALID_PARAMETER            = 87;
const WSAError OPERATION_ABORTED            = 995;
const WSAError IO_INCOMPLETE                = 996;
const WSAError IO_PENDING                   = 997;
const WSAError EINTR                        = 10004;
const WSAError EBADF                        = 10009;
const WSAError EACCESS                      = 10013;
const WSAError EFAULT                       = 10014;
const WSAError EINVAL                       = 10022;
const WSAError EMFILE                       = 10024;
const WSAError EWOULDBLOCK                  = 10035;
const WSAError EINPROGRESS                  = 10036;
const WSAError EALREADY                     = 10037;
const WSAError ENOTSOCK                     = 10038;
const WSAError EDESTADDRREQ                 = 10039;
const WSAError EMSGSIZE                     = 10040;
const WSAError EPROTOTYPE                   = 10041;
const WSAError ENOPROTOOPT                  = 10042;
const WSAError EPROTONOSUPPORT              = 10043;
const WSAError ESOCKTNOSUPPORT              = 10044;
const WSAError EOPNOTSUPP                   = 10045;
const WSAError EPFNOSUPPORT                 = 10046;
const WSAError EAFNOSUPPORT                 = 10047;
const WSAError EADDRINUSE                   = 10048;
const WSAError EADDRNOTAVAIL                = 10049;
const WSAError ENETDOWN                     = 10050;
const WSAError ENETUNREACH                  = 10051;
const WSAError ENETRESET                    = 10052;
const WSAError ECONNABORTED                 = 10053;
const WSAError ECONNRESET                   = 10054;
const WSAError ENOBUFS                      = 10055;
const WSAError EISCONN                      = 10056;
const WSAError ENOTCONN                     = 10057;
const WSAError ESHUTDOWN                    = 10058;
const WSAError ETOOMANYREFS                 = 10059;
const WSAError ETIMEDOUT                    = 10060;
const WSAError ECONNREFUSED                 = 10061;
const WSAError ELOOP                        = 10062;
const WSAError ENAMETOOLONG                 = 10063;
const WSAError EHOSTDOWN                    = 10064;
const WSAError EHOSTUNREACH                 = 10065;
const WSAError ENOTEMPTY                    = 10066;
const WSAError EPROCLIM                     = 10067;
const WSAError EUSERS                       = 10068;
const WSAError EDQUOT                       = 10069;
const WSAError ESTALE                       = 10070;
const WSAError EREMOTE                      = 10071;
const WSAError SYSNOTREADY                  = 10091;
const WSAError VERNOTSUPPORTED              = 10092;
const WSAError NOTINITIALISED               = 10093;
const WSAError EDISCON                      = 10101;
const WSAError ENOMORE                      = 10102;
const WSAError ECANCELLED                   = 10103;
const WSAError EINVALIDPROCTABLE            = 10104;
const WSAError EINVALIDPROVIDER             = 10105;
const WSAError EPROVIDERFAILEDINIT          = 10106;
const WSAError SYSCALLFAILURE               = 10107;
const WSAError SERVICE_NOT_FOUND            = 10108;
const WSAError TYPE_NOT_FOUND               = 10109;
const WSAError E_NO_MORE                    = 10110;
const WSAError E_CANCELLED                  = 10111;
const WSAError REFUSED                      = 10112;
const WSAError HOST_NOT_FOUND               = 11001;
const WSAError TRY_AGAIN                    = 11002;
const WSAError NO_RECOVERY                  = 11003;
const WSAError NO_DATA                      = 11004;
const WSAError QOS_RECEIVERS                = 11005;
const WSAError QOS_SENDERS                  = 11006;
const WSAError QOS_NO_SENDERS               = 11007;
const WSAError QOS_NO_RECEIVERS             = 11008;
const WSAError QOS_REQUEST_CONFIRMED        = 11009;
const WSAError QOS_ADMISSION_FAILURE        = 11010;
const WSAError QOS_POLICY_FAILURE           = 11011;
const WSAError QOS_BAD_STYLE                = 11012;
const WSAError QOS_BAD_OBJECT               = 11013;
const WSAError QOS_TRAFFIC_CTRL_ERROR       = 11014;
const WSAError QOS_GENERIC_ERROR            = 11015;
const WSAError QOS_ESERVICETYPE             = 11016;
const WSAError QOS_EFLOWSPEC                = 11017;
const WSAError QOS_EPROVSPECBUF             = 11018;
const WSAError QOS_EFILTERSTYLE             = 11019;
const WSAError QOS_EFILTERTYPE              = 11020;
const WSAError QOS_EFILTERCOUNT             = 11021;
const WSAError QOS_EOBJLENGTH               = 11022;
const WSAError QOS_EFLOWCOUNT               = 11023;
const WSAError QOS_EUNKOWNPSOBJ             = 11024;
const WSAError QOS_EPOLICYOBJ               = 11025;
const WSAError QOS_EFLOWDESC                = 11026;
const WSAError QOS_EPSFLOWSPEC              = 11027;
const WSAError QOS_EPSFILTERSPEC            = 11028;
const WSAError QOS_ESDMODEOBJ               = 11029;
const WSAError QOS_ESHAPERATEOBJ            = 11030;
const WSAError QOS_RESERVED_PETYPE          = 11031;
