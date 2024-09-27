// Source: https://github.com/c3lang/c3c/blob/master/lib/std/libc/os/posix.c3
// Permalink: https://github.com/c3lang/c3c/blob/800f797/lib/std/libc/os/posix.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module libc @if(env::POSIX);

extern fn isz recv(Fd socket, void *buffer, usz length, CInt flags);
extern fn isz send(Fd socket, void *buffer, usz length, CInt flags);

extern fn void* dlopen(ZString path, int flags);
extern fn CInt dlclose(void*);
extern fn void* dlsym(void* handle, ZString symbol);

const int RTLD_LAZY = 0x1;
const int RTLD_NOW = 0x2;
const int RTLD_LOCAL = 0x4;
const int RTLD_GLOBAL = 0x8;

def Pid_t = int;
def Uid_t = uint;
def Gid_t = uint;

const CUInt SA_ONSTACK       = env::LINUX ? 0x08000000 : 0x0001;
const CUInt SA_RESTART       = env::LINUX ? 0x10000000 : 0x0002;
const CUInt SA_RESETHAND     = env::LINUX ? 0x80000000 : 0x0004;
const CUInt SA_SIGINFO       = env::LINUX ? 0x00000004 : 0x0040;

def Sigset_t = uint @if(!env::LINUX);
def Sigset_t = ulong[16] @if(env::LINUX);
def SigActionFunction =  fn void(CInt, void*, void*);

struct Sigaction
{
	union
    {
    	SignalFunction      sa_handler;
    	SigActionFunction   sa_sigaction;
    }
	CInt                    sa_flags @if(env::FREEBSD);
	Sigset_t                sa_mask; // 128
	CInt                    sa_flags @if(!env::FREEBSD);
	void*                   sa_restorer @if(env::LINUX);
}

struct Stack_t
{
	void*   ss_sp;
	struct @if(!env::LINUX)
	{
		usz     ss_size;
		CInt    ss_flags;
	}
	struct @if(env::LINUX)
	{
		CInt    ss_flags;
		usz     ss_size;
	}
}

extern fn CInt sigaltstack(Stack_t* ss, Stack_t* old_ss);
extern fn CInt sigaction(CInt signum, Sigaction *action, Sigaction *oldaction);

module libc::termios @if(env::LIBC &&& env::POSIX);

distinct Cc = char;
distinct Speed = CUInt;
distinct Tcflags = CUInt;

const Tcflags TCOOFF = 0;
const Tcflags TCOON = 1;
const Tcflags TCIOFF = 2;
const Tcflags TCION = 3;
const Tcflags TCIFLUSH = 0;
const Tcflags TCOFLUSH = 1;
const Tcflags TCIOFLUSH = 2;
const Tcflags TCSANOW = 0;
const Tcflags TCSADRAIN = 1;
const Tcflags TCSAFLUSH = 2;
const Speed B0 = 0000000;
const Speed B50 = 0000001;
const Speed B75 = 0000002;
const Speed B110 = 0000003;
const Speed B134 = 0000004;
const Speed B150 = 0000005;
const Speed B200 = 0000006;
const Speed B300 = 0000007;
const Speed B600 = 0000010;
const Speed B1200 = 0000011;
const Speed B1800 = 0000012;
const Speed B2400 = 0000013;
const Speed B4800 = 0000014;
const Speed B9600 = 0000015;
const Speed B19200 = 0000016;
const Speed B38400 = 0000017;
const Speed B57600 = 0010001;
const Speed B115200 = 0010002;
const Speed B230400 = 0010003;
const Speed B460800 = 0010004;
const Speed B500000 = 0010005;
const Speed B576000 = 0010006;
const Speed B921600 = 0010007;
const Speed B1000000 = 0010010;
const Speed B1152000 = 0010011;
const Speed B1500000 = 0010012;
const Speed B2000000 = 0010013;
const Speed B2500000 = 0010014;
const Speed B3000000 = 0010015;
const Speed B3500000 = 0010016;
const Speed B4000000 = 0010017;
const Speed MAX_BAUD = B4000000;
const CInt VINTR = 0;
const CInt VQUIT = 1;
const CInt VERASE = 2;
const CInt VKILL = 3;
const CInt VEOF = 4;
const CInt VTIME = 5;
const CInt VMIN = 6;
const CInt VSWTC = 7;
const CInt VSTART = 8;
const CInt VSTOP = 9;
const CInt VSUSP = 10;
const CInt VEOL = 11;
const CInt VREPRINT = 12;
const CInt VDISCARD = 13;
const CInt VWERASE = 14;
const CInt VLNEXT = 15;
const CInt VEOL2 = 16;
const CInt ISIG = 0000001;
const CInt ICANON = 0000002;
const CInt ECHO = 0000010;
const CInt ECHOE = 0000020;
const CInt ECHOK = 0000040;
const CInt ECHONL = 0000100;
const CInt NOFLSH = 0000200;
const CInt TOSTOP = 0000400;
const CInt IEXTEN = 0100000;
const CInt CSIZE = 0000060;
const CInt CS5 = 0000000;
const CInt CS6 = 0000020;
const CInt CS7 = 0000040;
const CInt CS8 = 0000060;
const CInt CSTOPB = 0000100;
const CInt CREAD = 0000200;
const CInt PARENB = 0000400;
const CInt PARODD = 0001000;
const CInt HUPCL = 0002000;
const CInt CLOCAL = 0004000;
const CInt OPOST = 0000001;
const CInt OLCUC = 0000002;
const CInt ONLCR = 0000004;
const CInt OCRNL = 0000010;
const CInt ONOCR = 0000020;
const CInt ONLRET = 0000040;
const CInt OFILL = 0000100;
const CInt OFDEL = 0000200;
const CInt VTDLY = 0040000;
const CInt VT0 = 0000000;
const CInt VT1 = 0040000;
const CInt IGNBRK = 0000001;
const CInt BRKINT = 0000002;
const CInt IGNPAR = 0000004;
const CInt PARMRK = 0000010;
const CInt INPCK = 0000020;
const CInt ISTRIP = 0000040;
const CInt INLCR = 0000100;
const CInt IGNCR = 0000200;
const CInt ICRNL = 0000400;
const CInt IUCLC = 0001000;
const CInt IXON = 0002000;
const CInt IXANY = 0004000;
const CInt IXOFF = 0010000;
const CInt IMAXBEL = 0020000;
const CInt IUTF8 = 0040000;

extern fn CInt tcgetattr(Fd fd, Termios* self);
extern fn CInt tcsetattr(Fd fd, CInt optional_actions, Termios* self);
extern fn CInt tcsendbreak(Fd fd, CInt duration);
extern fn CInt tcdrain(Fd fd);
extern fn CInt tcflush(Fd fd, CInt queue_selector);
extern fn CInt tcflow(Fd fd, CInt action);
extern fn Speed cfgetospeed(Termios* self);
extern fn Speed cfgetispeed(Termios* self);
extern fn CInt cfsetospeed(Termios* self, Speed speed);
extern fn CInt cfsetispeed(Termios* self, Speed speed);

const CInt NCCS = 32;
struct Termios {
        Tcflags c_iflag;
        Tcflags c_oflag;
        Tcflags c_cflag;
        Tcflags c_lflag;
        Cc c_line;
        Cc[NCCS] c_cc;
        Speed c_ispeed;
        Speed c_ospeed;
} 

