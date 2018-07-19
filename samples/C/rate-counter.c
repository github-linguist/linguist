#include <stdio.h>
#include <time.h>

// We only get one-second precision on most systems, as
// time_t only holds seconds.
struct rate_state_s
{
    time_t lastFlush;
    time_t period;
    size_t tickCount;
};

void tic_rate(struct rate_state_s* pRate)
{
    pRate->tickCount += 1;

    time_t now = time(NULL);

    if((now - pRate->lastFlush) >= pRate->period)
    {
        //TPS Report
        size_t tps = 0.0;
        if(pRate->tickCount > 0)
            tps = pRate->tickCount / (now - pRate->lastFlush);

        printf("%u tics per second.\n", tps);

        //Reset
        pRate->tickCount = 0;
        pRate->lastFlush = now;
    }
}

// A stub function that simply represents whatever it is
// that we want to multiple times.
void something_we_do()
{
    // We use volatile here, as many compilers will optimize away
    // the for() loop otherwise, even without optimizations
    // explicitly enabled.
    //
    // volatile tells the compiler not to make any assumptions
    // about the variable, implying that the programmer knows more
    // about that variable than the compiler, in this case.
    volatile size_t anchor = 0;
    size_t x = 0;
    for(x = 0; x < 0xffff; ++x)
    {
        anchor = x;
    }
}

int main()
{
    time_t start = time(NULL);

    struct rate_state_s rateWatch;
    rateWatch.lastFlush = start;
    rateWatch.tickCount = 0;
    rateWatch.period = 5; // Report every five seconds.

    time_t latest = start;
    // Loop for twenty seconds
    for(latest = start; (latest - start) < 20; latest = time(NULL))
    {
        // Do something.
        something_we_do();

        // Note that we did something.
        tic_rate(&rateWatch);
    }

    return 0;
}
