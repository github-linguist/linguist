#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <sys/time.h>
#include <pthread.h>

/* no need to lock the object: at worst the readout would be 1 tick off,
   which is no worse than integrator's inate inaccuracy */
typedef struct {
	double (*func)(double);
	struct timeval start;
	double v, last_v, last_t;
	pthread_t id;
} integ_t, *integ;

void update(integ x)
{
	struct timeval tv;
	double t, v, (*f)(double);

	f = x->func;
	gettimeofday(&tv, 0);
	t = ((tv.tv_sec - x->start.tv_sec) * 1000000
		+ tv.tv_usec - x->start.tv_usec) * 1e-6;
	v = f ? f(t) : 0;
	x->v += (x->last_v + v) * (t - x->last_t) / 2;
	x->last_t = t;
}

void* tick(void *a)
{
	integ x = a;
	while (1) {
		usleep(100000); /* update every .1 sec */
		update(x);
	}
}

void set_input(integ x, double (*func)(double))
{
	update(x);
	x->func = func;
	x->last_t = 0;
	x->last_v = func ? func(0) : 0;
}

integ new_integ(double (*func)(double))
{
	integ x = malloc(sizeof(integ_t));
	x->v = x->last_v = 0;
	x->func = 0;
	gettimeofday(&x->start, 0);
	set_input(x, func);
	pthread_create(&x->id, 0, tick, x);
	return x;
}

double sine(double t) { return sin(4 * atan2(1, 1) * t); }

int main()
{
	integ x = new_integ(sine);
	sleep(2);
	set_input(x, 0);
	usleep(500000);
	printf("%g\n", x->v);

	return 0;
}
