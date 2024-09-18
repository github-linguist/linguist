// Source: https://github.com/c3lang/c3c/blob/master/lib/std/net/os/linux.c3
// Permalink: https://github.com/c3lang/c3c/blob/b8d77d2/lib/std/net/os/linux.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::net::os @if(env::LINUX);
import libc;

const AIFamily PLATFORM_AF_AX25         = 3;
const AIFamily PLATFORM_AF_IPX          = 4;
const AIFamily PLATFORM_AF_APPLETALK    = 5;
const AIFamily PLATFORM_AF_NETROM       = 6;
const AIFamily PLATFORM_AF_BRIDGE       = 7;
const AIFamily PLATFORM_AF_AAL5         = 8;
const AIFamily PLATFORM_AF_X25          = 9;
const AIFamily PLATFORM_AF_INET6        = 10;

const PLATFORM_O_NONBLOCK = 0o4000;

// https://github.com/torvalds/linux/blob/master/include/uapi/asm-generic/socket.h
const int SOL_SOCKET = 1;

const int SO_DEBUG                   = 1; // turn on debugging info recording
const int SO_REUSEADDR               = 2; // allow local address reuse
const int SO_TYPE                    = 3;
const int SO_ERROR                   = 4;
const int SO_DONTROUTE               = 5; // just use interface addresses
const int SO_BROADCAST               = 6; // permit sending of broadcast msgs
const int SO_SNDBUF                  = 7; // Send buffer size
const int SO_RCVBUF                  = 8; // Receive buffer size
const int SO_KEEPALIVE               = 9; // keep connections alive
const int SO_OOBINLINE	             = 10; // leave received OOB data in line
const int SO_NO_CHECK                = 11;
const int SO_PRIORITY                = 12;
const int SO_LINGER	                 = 13; // linger on close if data present (in ticks)
const int SO_BSDCOMPAT               = 14;
const int SO_REUSEPORT               = 15; // allow local address & port reuse
const int SO_RCVLOWAT                = 18;
const int SO_SNDLOWAT                = 19;
const int SO_RCVTIMEO                = 20; // IMPORTANT Verify before use
const int SO_SNDTIMEO                = 21; // IMPORTANT Verify before use
const int SO_BINDTODEVICE            = 25;
const int SO_ATTACH_FILTER           = 26;
const int SO_DETACH_FILTER           = 27;
const int SO_PEERNAME                = 28;
const int SO_TIMESTAMP	             = 29; // IMPORTANT Verify before use timestamp received dgram traffic
const int SO_ACCEPTCONN              = 30;
const int SO_PEERSEC                 = 31;
const int SO_SNDBUFFORCE             = 32;
const int SO_RCVBUFFORCE             = 33;
const int SO_PASSSEC                 = 34;
const int SO_MARK                    = 36;
const int SO_PROTOCOL                = 38;
const int SO_DOMAIN                  = 39;
const int SO_RXQ_OVFL                = 40;
const int SO_WIFI_STATUS             = 41;
const int SO_PEEK_OFF                = 42;
const int SO_NOFCS                   = 43;
const int SO_LOCK_FILTER             = 44;
const int SO_SELECT_ERR_QUEUE        = 45;
const int SO_BUSY_POLL               = 46;
const int SO_MAX_PACING_RATE         = 47;
const int SO_BPF_EXTENSIONS          = 48;
const int SO_INCOMING_CPU            = 49;
const int SO_ATTACH_BPF              = 50;
const int SO_ATTACH_REUSEPORT_CBPF   = 51;
const int SO_ATTACH_REUSEPORT_EBPF   = 52;
const int SO_CNX_ADVICE              = 53;
const int SO_MEMINFO                 = 55;
const int SO_INCOMING_NAPI_ID        = 56;
const int SO_COOKIE                  = 57;
const int SO_PEERGROUPS              = 59;
const int SO_ZEROCOPY                = 60;
const int SO_TXTIME                  = 61;
const int SO_BINDTOIFINDEX           = 62;
const int SO_DETACH_REUSEPORT_BPF    = 68;
const int SO_PREFER_BUSY_POLL        = 69;
const int SO_BUSY_POLL_BUDGET        = 70;
const int SO_NETNS_COOKIE            = 71;
const int SO_BUF_LOCK                = 72;
const int SO_RESERVE_MEM             = 73;
const int SO_TXREHASH                = 74;
const int SO_RCVMARK                 = 75;
const int SO_PASSPIDFD               = 76;
const int SO_PEERPIDFD               = 77;

const CUShort POLLRDNORM              = 0x0040;
const CUShort POLLRDBAND              = 0x0080;
const CUShort POLLWRNORM              = 0x0100;
const CUShort POLLWRBAND              = 0x0200;
const CUShort POLLMSG                 = 0x0400;
const CUShort POLLREMOVE              = 0x1000;
const CUShort POLLRDHUP               = 0x2000;
const CUShort POLLFREE                = 0x4000;
const CUShort POLL_BUSY_LOOP          = 0x8000;
