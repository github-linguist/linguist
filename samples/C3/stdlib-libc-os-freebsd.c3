// Source: https://github.com/c3lang/c3c/blob/master/lib/std/libc/os/freebsd.c3
// Permalink: https://github.com/c3lang/c3c/blob/1b54a99/lib/std/libc/os/freebsd.c3
// Generated: 2024-09-15 14:16:14 UTC (UTC)
// License: MIT

module libc @if(env::FREEBSD);

// Checked for x86_64, this is notoriously incorrect when comparing with Rust code etc

def Blksize_t = $typefrom(env::X86_64 ? long.typeid : CInt.typeid);
def Nlink_t = $typefrom(env::X86_64 ? ulong.typeid : CUInt.typeid);
def Blkcnt_t = long;
def Ino_t = ulong;
def Dev_t = ulong;
def Mode_t = uint;
def Ino64_t = ulong;
def Blkcnt64_t = long;

struct Stat @if(env::X86_64)
{
	Dev_t st_dev;
	Ino_t st_ino;
	Nlink_t st_nlink;
	Mode_t st_mode;
	Uid_t st_uid;
	Gid_t st_gid;
	CInt __pad0;
	Dev_t st_rdev;
	Off_t st_size;
	Blksize_t st_blksize;
	Blkcnt_t st_blocks;
	Time_t st_atime;
	long st_atime_nsec;
	Time_t st_mtime;
	long st_mtime_nsec;
	Time_t st_ctime;
	long st_ctime_nsec;
	long[3] __unused;
}

struct Stat @if(!env::X86_64)
{
	Dev_t st_dev;
	Ino_t st_ino;
	Mode_t st_mode;
	Nlink_t st_nlink;
	Uid_t st_uid;
	Gid_t st_gid;
	Dev_t st_rdev;
	CInt __pad1;
	Off_t st_size;
	Blksize_t st_blksize;
	CInt __pad2;
	Blkcnt_t st_blocks;
	Time_t st_atime;
	long st_atime_nsec;
	Time_t st_mtime;
	long st_mtime_nsec;
	Time_t st_ctime;
	long st_ctime_nsec;
	CInt[2] __unused;
}

extern fn CInt stat(ZString path, Stat* stat);

extern fn CInt get_nprocs();
extern fn CInt get_nprocs_conf();
