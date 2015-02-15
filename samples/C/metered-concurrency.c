#include <semaphore.h>
#include <pthread.h>
#include <stdlib.h>
#include <stdio.h>
#include <unistd.h>

sem_t sem;
int count = 3;

/* the whole point of a semaphore is that you don't count it:
 * p/v are atomic.  Unless it's locked while you are doing
 * something with the count, the value is only informative */
#define getcount() count
void acquire()
{
	sem_wait(&sem);
	count--;
}

void release()
{
	count++;
	sem_post(&sem);
}

void* work(void * id)
{
	int i = 10;
	while (i--) {
		acquire();
		printf("#%d acquired sema at %d\n", *(int*)id, getcount());
		usleep(rand() % 4000000); /* sleep 2 sec on average */
		release();
		usleep(0);  /* effectively yield */
	}
	return 0;
}

int main()
{
	pthread_t th[4];
	int i, ids[] = {1, 2, 3, 4};

	sem_init(&sem, 0, count);

	for (i = 4; i--;) pthread_create(th + i, 0, work, ids + i);
	for (i = 4; i--;) pthread_join(th[i], 0);
	printf("all workers done\n");

	return sem_destroy(&sem);
}
