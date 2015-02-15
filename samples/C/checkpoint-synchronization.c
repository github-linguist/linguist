#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <omp.h>

int main()
{
        int jobs = 41, tid;
        omp_set_num_threads(5);

        #pragma omp parallel shared(jobs) private(tid)
        {
                tid = omp_get_thread_num();
                while (jobs > 0) {
                        /* this is the checkpoint */
                        #pragma omp barrier
                        if (!jobs) break;

                        printf("%d: taking job %d\n", tid, jobs--);
                        usleep(100000 + rand() / (double) RAND_MAX * 3000000);
                        printf("%d: done job\n", tid);
                }

                printf("[%d] leaving\n", tid);

                /* this stops jobless thread from exiting early and killing workers */
                #pragma omp barrier
        }

        return 0;
}
