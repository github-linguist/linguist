// Source: https://github.com/c3lang/c3c/blob/master/lib/std/net/os/common.c3
// Permalink: https://github.com/c3lang/c3c/blob/99cfaa1/lib/std/net/os/common.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module std::net::os;
const bool SUPPORTS_INET = env::LIBC && (env::WIN32 || env::DARWIN || env::LINUX);

distinct AIFamily = CInt;
distinct AIProtocol = CInt;
distinct AISockType = CInt;
distinct AIFlags = CInt;

def Socklen_t = CUInt @if(!env::WIN32);
def Socklen_t = usz @if(env::WIN32);

distinct SockAddrPtr = void*;

struct AddrInfo
{
	AIFlags ai_flags;
	AIFamily ai_family;
	AISockType ai_socktype;
	AIProtocol ai_protocol;
	Socklen_t ai_addrlen;
	struct @if(env::WIN32 || env::DARWIN)
	{
		ZString ai_canonname;
		SockAddrPtr ai_addr;
	}
	struct @if(env::LINUX)
	{
		SockAddrPtr ai_addr;
		ZString ai_canonname;
	}
	AddrInfo* ai_next;
}


const PLATFORM_O_NONBLOCK @if(!$defined(PLATFORM_O_NONBLOCK)) = 0;

const AISockType SOCK_STREAM                    = 1; // Stream
const AISockType SOCK_DGRAM                     = 2; // Datagram
const AISockType SOCK_RAW                       = 3; // Raw
const AISockType SOCK_RDM                       = 4; // Reliably delivered
const AISockType SOCK_SEQPACKET                 = 5; // Sequential packet

const AIFlags AI_PASSIVE                        = 0x1;
const AIFlags AI_CANONNAME                      = 0x2;
const AIFlags AI_NUMERICHOST                    = 0x4;

const AIFamily AF_UNSPEC                        = 0;
const AIFamily AF_UNIX                          = 1;
const AIFamily AF_INET                          = 2;
const AIFamily AF_INET6                         = PLATFORM_AF_INET6;
const AIFamily AF_IPX                           = PLATFORM_AF_IPX;
const AIFamily AF_APPLETALK                     = PLATFORM_AF_APPLETALK;

const O_NONBLOCK = PLATFORM_O_NONBLOCK;

extern fn CInt getaddrinfo(ZString nodename, ZString servname, AddrInfo* hints, AddrInfo** res) @if(SUPPORTS_INET);
extern fn void freeaddrinfo(AddrInfo* res) @if(SUPPORTS_INET);
extern fn CInt setsockopt(NativeSocket socket, CInt level, CInt optname, void* optval, Socklen_t optlen) @if(SUPPORTS_INET);
extern fn CInt getsockopt(NativeSocket socket, CInt level, CInt optname, void* optval, Socklen_t optlen) @if(SUPPORTS_INET);

module std::net::os @if(!env::LIBC || !(env::WIN32 || env::DARWIN || env::LINUX));

const AIFamily PLATFORM_AF_INET6        = 0;
const AIFamily PLATFORM_AF_IPX          = 0;
const AIFamily PLATFORM_AF_APPLETALK    = 0;

const IPPPOTO_IP = 0;
const IPPROTO_ICMP = 1;
const IPPROTO_IGMP = 2;
const IPPROTO_IPV4 = 4;
const IPPROTO_TCP = 6;
const IPPROTO_ST = 7;
const IPPROTO_EGP = 8;
const IPPROTO_PIGP = 9;
const IPPROTO_RCCMON = 10;
const IPPROTO_NVPII = 11;
const IPPROTO_PUP = 12;
const IPPROTO_ARGUS = 13;
const IPPROTO_EMCON = 14;
const IPPROTO_XNET = 15;
const IPPROTO_CHAOS = 16;
const IPPROTO_UDP = 17;
const IPPROTO_MUX = 18;
const IPPROTO_MEAS = 19;
const IPPROTO_HMP = 20;
const IPPROTO_PRM = 21;
const IPPROTO_IDP = 22;
const IPPROTO_TRUNK1 = 23;
const IPPROTO_TRUNK2 = 24;
const IPPROTO_LEAF1 = 25;
const IPPROTO_LEAF2 = 26;
const IPPROTO_RDP = 27;
const IPPROTO_IRTP = 28;
const IPPROTO_TP = 29;
const IPPROTO_BLT = 30;
const IPPROTO_NSP = 31;
const IPPROTO_INP = 32;
const IPPROTO_SEP = 33;
const IPPROTO_3PC = 34;
const IPPROTO_IDPR = 35;
const IPPROTO_XTP = 36;
const IPPROTO_DDP = 37;
const IPPROTO_CMTP = 38;
const IPPROTO_TPXX = 39;
const IPPROTO_IL = 40;
const IPPROTO_IPV6 = 41;
const IPPROTO_SDRP = 42;
const IPPROTO_ROUTING = 43;
const IPPROTO_FRAGMENT = 44;
const IPPROTO_IDRP = 45;
const IPPROTO_RSVP = 46;
const IPPROTO_GRE = 47;
const IPPROTO_MHRP = 48;
const IPPROTO_BHA = 49;
const IPPROTO_ESP = 50;
const IPPROTO_AH = 51;
const IPPROTO_INLSP = 52;
const IPPROTO_SWIPE = 53;
const IPPROTO_NHRP = 54;
const IPPROTO_ICMPV6 = 58;
const IPPROTO_NONE = 59;
const IPPROTO_DSTOPTS = 60;
const IPPROTO_AHIP = 61;
const IPPROTO_CFTP = 62;
const IPPROTO_HELLO = 63;
const IPPROTO_SATEXPAK = 64;
const IPPROTO_KRYPTOLAN = 65;
const IPPROTO_RVD = 66;
const IPPROTO_IPPC = 67;
const IPPROTO_ADFS = 68;
const IPPROTO_SATMON = 69;
const IPPROTO_VISA = 70;
const IPPROTO_IPCV = 71;
const IPPROTO_CPNX = 72;
const IPPROTO_CPHB = 73;
const IPPROTO_WSN = 74;
const IPPROTO_PVP = 75;
const IPPROTO_BRSATMON = 76;
const IPPROTO_ND = 77;
const IPPROTO_WBMON = 78;
const IPPROTO_WBEXPAK = 79;
const IPPROTO_EON = 80;
const IPPROTO_VMTP = 81;
const IPPROTO_SVMTP = 82;
const IPPROTO_VINES = 83;
const IPPROTO_TTP = 84;
const IPPROTO_IGP = 85;
const IPPROTO_DGP = 86;
const IPPROTO_TCF = 87;
const IPPROTO_IGRP = 88;
const IPPROTO_OSPFIGP = 89;
const IPPROTO_SRPC = 90;
const IPPROTO_LARP = 91;
const IPPROTO_MTP = 92;
const IPPROTO_AX25 = 93;
const IPPROTO_IPEIP = 94;
const IPPROTO_MICP = 95;
const IPPROTO_SCCSP = 96;
const IPPROTO_ETHERIP = 97;
const IPPROTO_ENCAP = 98;
const IPPROTO_APES = 99;
const IPPROTO_GMTP = 100;
const IPPROTO_PIM = 103;
const IPPROTO_IPCOMP = 108;
const IPPROTO_PGM = 113;
const IPPROTO_SCTP = 132;
const IPPROTO_DIVERT = 254;
const IPPROTO_RAW = 255;