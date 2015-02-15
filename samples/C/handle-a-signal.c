#include <stdio.h>
#include <stdlib.h>	// for exit()
#include <signal.h>
#include <time.h>	// for clock()
#include <unistd.h>	// for POSIX usleep()

volatile sig_atomic_t gotint = 0;

void handleSigint() {
    /*
     * Signal safety: It is not safe to call clock(), printf(),
     * or exit() inside a signal handler. Instead, we set a flag.
     */
    gotint = 1;
}

int main() {
    clock_t startTime = clock();
    signal(SIGINT, handleSigint);
    int i=0;
    for (;;) {
        if (gotint)
            break;
        usleep(500000);
        if (gotint)
            break;
	printf("%d\n", ++i);
    }
    clock_t endTime = clock();
    double td = (endTime - startTime) / (double)CLOCKS_PER_SEC;
    printf("Program has run for %5.3f seconds\n", td);
    return 0;
}
