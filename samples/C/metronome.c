#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <stdint.h>
#include <signal.h>
#include <time.h>
#include <sys/time.h>

struct timeval start, last;

inline int64_t tv_to_u(struct timeval s)
{
	return s.tv_sec * 1000000 + s.tv_usec;
}

inline struct timeval u_to_tv(int64_t x)
{
	struct timeval s;
	s.tv_sec = x / 1000000;
	s.tv_usec = x % 1000000;
	return s;
}

void draw(int dir, int64_t period, int64_t cur, int64_t next)
{
	int len = 40 * (next - cur) / period;
	int s, i;

	if (len > 20) len = 40 - len;
	s = 20 + (dir ? len : -len);

	printf("\033[H");
	for (i = 0; i <= 40; i++) putchar(i == 20 ? '|': i == s ? '#' : '-');
}

void beat(int delay)
{
	struct timeval tv = start;
	int dir = 0;
	int64_t d = 0, corr = 0, slp, cur, next = tv_to_u(start) + delay;
	int64_t draw_interval = 20000;
	printf("\033[H\033[J");
	while (1) {
		gettimeofday(&tv, 0);
		slp = next - tv_to_u(tv) - corr;
		usleep(slp);
		gettimeofday(&tv, 0);

		putchar(7); /* bell */
		fflush(stdout);

		printf("\033[5;1Hdrift: %d compensate: %d (usec)   ",
			(int)d, (int)corr);
		dir = !dir;

		cur = tv_to_u(tv);
		d = cur - next;
		corr = (corr + d) / 2;
		next += delay;

		while (cur + d + draw_interval < next) {
			usleep(draw_interval);
			gettimeofday(&tv, 0);
			cur = tv_to_u(tv);
			draw(dir, delay, cur, next);
			fflush(stdout);
		}
	}
}

int main(int c, char**v)
{
	int bpm;

	if (c < 2 || (bpm = atoi(v[1])) <= 0) bpm = 60;
	if (bpm > 600) {
		fprintf(stderr, "frequency %d too high\n", bpm);
		exit(1);
	}

	gettimeofday(&start, 0);
	last = start;
	beat(60 * 1000000 / bpm);

	return 0;
}
