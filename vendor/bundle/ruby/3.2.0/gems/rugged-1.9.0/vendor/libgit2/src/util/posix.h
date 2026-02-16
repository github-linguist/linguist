/*
 * Copyright (C) the libgit2 contributors. All rights reserved.
 *
 * This file is part of libgit2, distributed under the GNU GPL v2 with
 * a Linking Exception. For full terms see the included COPYING file.
 */
#ifndef INCLUDE_posix_h__
#define INCLUDE_posix_h__

#include "git2_util.h"

#include <stdlib.h>
#include <fcntl.h>
#include <time.h>

/* stat: file mode type testing macros */
#ifndef S_IFGITLINK
#define S_IFGITLINK 0160000
#define S_ISGITLINK(m) (((m) & S_IFMT) == S_IFGITLINK)
#endif

#ifndef S_IFLNK
#define S_IFLNK 0120000
#undef _S_IFLNK
#define _S_IFLNK S_IFLNK
#endif

#ifndef S_IWUSR
#define S_IWUSR 00200
#endif

#ifndef S_IXUSR
#define S_IXUSR 00100
#endif

#ifndef S_ISLNK
#define S_ISLNK(m) (((m) & _S_IFMT) == _S_IFLNK)
#endif

#ifndef S_ISDIR
#define S_ISDIR(m) (((m) & _S_IFMT) == _S_IFDIR)
#endif

#ifndef S_ISREG
#define S_ISREG(m) (((m) & _S_IFMT) == _S_IFREG)
#endif

#ifndef S_ISFIFO
#define S_ISFIFO(m) (((m) & _S_IFMT) == _S_IFIFO)
#endif

/* if S_ISGID is not defined, then don't try to set it */
#ifndef S_ISGID
#define S_ISGID 0
#endif

#ifndef O_BINARY
#define O_BINARY 0
#endif
#ifndef O_CLOEXEC
#define O_CLOEXEC 0
#endif
#ifndef SOCK_CLOEXEC
#define SOCK_CLOEXEC 0
#endif

/* access() mode parameter #defines	*/
#ifndef F_OK
#define F_OK 0 /* existence check */
#endif
#ifndef W_OK
#define W_OK 2 /* write mode check */
#endif
#ifndef R_OK
#define R_OK 4 /* read mode check */
#endif

/* Determine whether an errno value indicates that a read or write failed
 * because the descriptor is blocked.
 */
#if defined(EWOULDBLOCK)
#define GIT_ISBLOCKED(e) ((e) == EAGAIN || (e) == EWOULDBLOCK)
#else
#define GIT_ISBLOCKED(e) ((e) == EAGAIN)
#endif

/* define some standard errnos that the runtime may be missing.  for example,
 * mingw lacks EAFNOSUPPORT. */
#ifndef EAFNOSUPPORT
#define EAFNOSUPPORT (INT_MAX-1)
#endif

/* Compiler independent macro to handle signal interrpted system calls */
#define HANDLE_EINTR(result, x) do {					\
		result = (x);						\
	} while (result == -1 && errno == EINTR);


/* Provide a 64-bit size for offsets. */

#if defined(_MSC_VER)
typedef __int64 off64_t;
#elif defined(__HAIKU__)
typedef __haiku_std_int64 off64_t;
#elif defined(__APPLE__)
typedef __int64_t off64_t;
#elif defined(_AIX)
typedef long long off64_t;
#else
typedef int64_t off64_t;
#endif

typedef int git_file;

/**
 * Standard POSIX Methods
 *
 * All the methods starting with the `p_` prefix are
 * direct ports of the standard POSIX methods.
 *
 * Some of the methods are slightly wrapped to provide
 * saner defaults. Some of these methods are emulated
 * in Windows platforms.
 *
 * Use your manpages to check the docs on these.
 */

extern ssize_t p_read(git_file fd, void *buf, size_t cnt);
extern int p_write(git_file fd, const void *buf, size_t cnt);

extern ssize_t p_pread(int fd, void *data, size_t size, off64_t offset);
extern ssize_t p_pwrite(int fd, const void *data, size_t size, off64_t offset);

#define p_close(fd) close(fd)
#define p_umask(m) umask(m)

extern int p_open(const char *path, int flags, ...);
extern int p_creat(const char *path, mode_t mode);
extern int p_getcwd(char *buffer_out, size_t size);
extern int p_rename(const char *from, const char *to);

extern int git__page_size(size_t *page_size);
extern int git__mmap_alignment(size_t *page_size);

/* The number of times `p_fsync` has been called.  Note that this is for
 * test code only; it it not necessarily thread-safe and should not be
 * relied upon in production.
 */
extern size_t p_fsync__cnt;

/**
 * Platform-dependent methods
 */
#ifdef GIT_WIN32
#	include "win32/posix.h"
#else
#	include "unix/posix.h"
#endif

#include "strnlen.h"

#ifdef NO_READDIR_R
GIT_INLINE(int) p_readdir_r(DIR *dirp, struct dirent *entry, struct dirent **result)
{
	GIT_UNUSED(entry);
	*result = readdir(dirp);
	return 0;
}
#else /* NO_READDIR_R */
#	define p_readdir_r(d,e,r) readdir_r(d,e,r)
#endif

#ifdef NO_ADDRINFO
#	include <netdb.h>
struct addrinfo {
	struct hostent *ai_hostent;
	struct servent *ai_servent;
	struct sockaddr_in ai_addr_in;
	struct sockaddr *ai_addr;
	size_t ai_addrlen;
	int ai_family;
	int ai_socktype;
	int ai_protocol;
	long ai_port;
	struct addrinfo *ai_next;
};

extern int p_getaddrinfo(const char *host, const char *port,
	struct addrinfo *hints, struct addrinfo **info);
extern void p_freeaddrinfo(struct addrinfo *info);
extern const char *p_gai_strerror(int ret);
#else
#	define p_getaddrinfo(a, b, c, d) getaddrinfo(a, b, c, d)
#	define p_freeaddrinfo(a) freeaddrinfo(a)
#	define p_gai_strerror(c) gai_strerror(c)
#endif /* NO_ADDRINFO */

#ifdef GIT_IO_POLL
# include <poll.h>
# define p_poll poll
#elif GIT_IO_WSAPOLL
# include <winsock2.h>
# define p_poll WSAPoll
#else
# define POLLIN  0x01
# define POLLPRI 0x02
# define POLLOUT 0x04
# define POLLERR 0x08
# define POLLHUP 0x10

struct pollfd {
	int fd;
	short events;
	short revents;
};

extern int p_poll(struct pollfd *fds, unsigned int nfds, int timeout);
#endif

#endif
