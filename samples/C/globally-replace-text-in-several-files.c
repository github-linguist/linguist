#include <stdio.h>
#include <stdlib.h>
#include <stddef.h>
#include <string.h>
#include <sys/types.h>
#include <fcntl.h>
#include <sys/stat.h>
#include <unistd.h>
#include <err.h>
#include <string.h>

char * find_match(const char *buf, const char * buf_end, const char *pat, size_t len)
{
	ptrdiff_t i;
	char *start = buf;
	while (start + len < buf_end) {
		for (i = 0; i < len; i++)
			if (start[i] != pat[i]) break;

		if (i == len) return (char *)start;
		start++;
	}
	return 0;
}

int replace(const char *from, const char *to, const char *fname)
{
#define bail(msg) { warn(msg" '%s'", fname); goto done; }
	struct stat st;
	int ret = 0;
	char *buf = 0, *start, *end;
	size_t len = strlen(from), nlen = strlen(to);
	int fd = open(fname, O_RDWR);

	if (fd == -1) bail("Can't open");
	if (fstat(fd, &st) == -1) bail("Can't stat");
	if (!(buf = malloc(st.st_size))) bail("Can't alloc");
	if (read(fd, buf, st.st_size) != st.st_size) bail("Bad read");

	start = buf;
	end = find_match(start, buf + st.st_size, from, len);
	if (!end) goto done; /* no match found, don't change file */

	ftruncate(fd, 0);
	lseek(fd, 0, 0);
	do {
		write(fd, start, end - start);	/* write content before match */
		write(fd, to, nlen);		/* write replacement of match */
		start = end + len;		/* skip to end of match */
						/* find match again */
		end = find_match(start, buf + st.st_size, from, len);
	} while (end);

	/* write leftover after last match */
	if (start < buf + st.st_size)
		write(fd, start, buf + st.st_size - start);

done:
	if (fd != -1) close(fd);
	if (buf) free(buf);
	return ret;
}

int main()
{
	const char *from = "Goodbye, London!";
	const char *to   = "Hello, New York!";
	const char * files[] = { "test1.txt", "test2.txt", "test3.txt" };
	int i;

	for (i = 0; i < sizeof(files)/sizeof(char*); i++)
		replace(from, to, files[i]);

	return 0;
}
