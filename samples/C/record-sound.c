#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <fcntl.h>

void * record(size_t bytes)
{
	int fd;
	if (-1 == (fd = open("/dev/dsp", O_RDONLY))) return 0;
	void *a = malloc(bytes);
	read(fd, a, bytes);
	close(fd);
	return a;
}

int play(void *buf, size_t len)
{
	int fd;
	if (-1 == (fd = open("/dev/dsp", O_WRONLY))) return 0;
	write(fd, buf, len);
	close(fd);
	return 1;
}

int main()
{
	void *p = record(65536);
	play(p, 65536);
	return 0;
}
