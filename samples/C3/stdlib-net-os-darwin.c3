// Source: https://github.com/c3lang/c3c/blob/master/lib/std/net/os/darwin.c3
// Permalink: https://github.com/c3lang/c3c/blob/d1bb9c5/lib/std/net/os/darwin.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::net::os @if(env::DARWIN);
import libc;

const AIFlags AI_NUMERICSERV = 0x1000;
const AIFlags AI_ALL = 0x100;
const AIFlags AI_V4MAPPED_CFG = 0x200;
const AIFlags AI_ADDRCONFIG = 0x400;
const AIFlags AI_V4MAPPED = 0x800;
const AIFlags AI_UNUSABLE = 0x10000000;
const AIFlags AI_DEFAULT = AI_V4MAPPED_CFG | AI_ADDRCONFIG;

const AIFamily PLATFORM_AF_IMPLINK         = 3;
const AIFamily PLATFORM_AF_PUP             = 4;
const AIFamily PLATFORM_AF_CHAOS           = 5;
const AIFamily PLATFORM_AF_NS              = 6;
const AIFamily PLATFORM_AF_ISO             = 7;
const AIFamily PLATFORM_AF_ECMA            = 8;
const AIFamily PLATFORM_AF_DATAKIT         = 9;
const AIFamily PLATFORM_AF_CCITT           = 10;
const AIFamily PLATFORM_AF_SNA             = 11;
const AIFamily PLATFORM_AF_DECNET          = 12;
const AIFamily PLATFORM_AF_DLI             = 13;
const AIFamily PLATFORM_AF_LAT             = 14;
const AIFamily PLATFORM_AF_HYLINK          = 15;
const AIFamily PLATFORM_AF_APPLETALK       = 16;
const AIFamily PLATFORM_AF_ROUTE           = 17;
const AIFamily PLATFORM_AF_LINK            = 18;
const AIFamily PLATFORM_PSEUDO_AF_XTP      = 19;
const AIFamily PLATFORM_AF_COIP            = 20;
const AIFamily PLATFORM_AF_CNT             = 21;
const AIFamily PLATFORM_PSEUDO_AF_RTIP     = 22;
const AIFamily PLATFORM_AF_IPX             = 23;
const AIFamily PLATFORM_AF_SIP             = 24;
const AIFamily PLATFORM_PSEUDO_AF_PIP      = 25;
const AIFamily PLATFORM_AF_NDRV            = 27;
const AIFamily PLATFORM_AF_ISDN            = 28;
const AIFamily PLATFORM_PSEUDO_AF_KEY      = 29;
const AIFamily PLATFORM_AF_INET6           = 30;
const AIFamily PLATFORM_AF_NATM            = 31;
const AIFamily PLATFORM_AF_SYSTEM          = 32;
const AIFamily PLATFORM_AF_NETBIOS         = 33;
const AIFamily PLATFORM_AF_PPP             = 34;
const AIFamily PLATFORM_PSEUDO_AF_HDRCMPLT = 35;
const AIFamily PLATFORM_AF_IEEE80211       = 37;
const AIFamily PLATFORM_AF_UTUN            = 38;
const AIFamily PLATFORM_AF_VSOCK           = 40;
const AIFamily PLATFORM_AF_MAX             = 41;

const int PLATFORM_O_NONBLOCK = 0x04;

// https://opensource.apple.com/source/xnu/xnu-4570.41.2/bsd/sys/socket.h.auto.html
const int SOL_SOCKET             = 0xffff;
const int SO_DEBUG               = 0x0001; // turn on debugging info recording
const int SO_ACCEPTCONN          = 0x0002; // socket has had listen()
const int SO_REUSEADDR           = 0x0004; // allow local address reuse
const int SO_KEEPALIVE           = 0x0008; // keep connections alive
const int SO_DONTROUTE           = 0x0010; // just use interface addresses
const int SO_BROADCAST           = 0x0020; // permit sending of broadcast msgs
const int SO_USELOOPBACK         = 0x0040; // bypass hardware when possible
const int SO_LINGER	             = 0x0080; // linger on close if data present (in ticks)
const int SO_OOBINLINE	         = 0x0100; // leave received OOB data in line
const int SO_REUSEPORT           = 0x0200; // allow local address & port reuse
const int SO_TIMESTAMP	         = 0x0400; // timestamp received dgram traffic
const int SO_TIMESTAMP_MONOTONIC = 0x0800; // Monotonically increasing timestamp on rcvd dgram
const int SO_DONTTRUNC           = 0x2000; // Apple Retain unread data
const int SO_WANTMORE            = 0x4000; // Apple: Give hint when more data ready
const int SO_WANTOOBFLAG         = 0x8000; // Apple: Want OOB in MSG_FLAG on receive

const int SO_SNDBUF              = 0x1001; // Send buffer size
const int SO_RCVBUF              = 0x1002; // Recieve buffer size
const int SO_SNDLOWAT            = 0x1003; // Send low-water mark
const int SO_RCVLOWAT            = 0x1004; // Receive low-water mark
const int SO_SNDTIMEO            = 0x1005; // Send timeout
const int SO_RCVTIMEO            = 0x1006; // Receive timeout
const int SO_ERROR               = 0x1007; // Get error status and clear
const int SO_TYPE                = 0x1008; // Socket type
const int SO_LABEL               = 0x1010; // Socket MAC label
const int SO_PEERLABEL           = 0x1011; // Socket peer MAC label
const int SO_NREAD               = 0x1020; // Apple: get 1st-packet byte count
const int SO_NKE                 = 0x1021; // Apple: Install socket-level NKE
const int SO_NOSIGPIPE           = 0x1022; // Apple: No SIGPIPE on EPIPE
const int SO_NOADDRERR           = 0x1023; // Apple: Returns EADDRNOTAVAIL when src is not available anymore
const int SO_NWRITE              = 0x1024; // Apple: Get number of bytes currently in send socket buffer
const int SO_REUSESHAREUID       = 0x1025; // Apple: Allow reuse of port/socket by different userids
const int SO_LINGER_SEC          = 0x1080; // linger on close if data present (in seconds)


const CShort POLLRDNORM          = 0x0040;
const CShort POLLRDBAND          = 0x0080;
const CShort POLLWRNORM          = POLLOUT;
const CShort POLLWRBAND          = 0x0100;
const CShort POLLEXTEND          = 0x0200; // file may have been extended
const CShort POLLATTRIB          = 0x0400; // file attributes may have changed
const CShort POLLNLINK           = 0x0800; // (un)link/rename may have happened
const CShort POLLWRITE           = 0x1000; // file's contents may have changed

