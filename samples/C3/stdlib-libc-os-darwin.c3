// Source: https://github.com/c3lang/c3c/blob/master/lib/std/libc/os/darwin.c3
// Permalink: https://github.com/c3lang/c3c/blob/65bea1c/lib/std/libc/os/darwin.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module libc @if(env::DARWIN);

def Dev_t = int;
def Mode_t = ushort;
def Nlink_t = ushort;
def Blkcnt_t = long;
def Blksize_t = int;
def Ino_t = ulong;

struct Stat
{
	Dev_t st_dev;
	Mode_t st_mode;
	Nlink_t st_nlink;
	Ino_t st_ino;
	Uid_t st_uid;
	Gid_t st_gid;
	Dev_t st_rdev;

	Timespec st_atimespec; // time of last access
	Timespec st_mtimespec; // time of last data modification
	Timespec st_ctimespec; // time of last status change
	Timespec st_birthtimespec; // time of file creation(birth)
	Off_t st_size; // file size, in bytes
	Blkcnt_t st_blocks; // blocks allocated for file
	Blksize_t st_blocksize; // optimal blocksize for I/O
	uint st_flags; // user defined flags for file
	uint st_gen; // file generation number
	int st_lspare; // RESERVED
	long[2] st_qspare; // RESERVED
}

extern fn int stat(ZString str, Stat* stat) @extern("stat64");

extern fn CInt sysctl(CInt *name, CUInt namelen, void *oldp, usz *oldlenp, void *newp, usz newlen);
