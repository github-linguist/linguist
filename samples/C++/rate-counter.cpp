#include <iostream>
#include <ctime>

// We only get one-second precision on most systems, as
// time_t only holds seconds.
class CRateState
{
protected:
    time_t m_lastFlush;
    time_t m_period;
    size_t m_tickCount;
public:
    CRateState(time_t period);
    void Tick();
};

CRateState::CRateState(time_t period) : m_lastFlush(std::time(NULL)),
                                        m_period(period),
                                        m_tickCount(0)
{ }

void CRateState::Tick()
{
    m_tickCount++;

    time_t now = std::time(NULL);

    if((now - m_lastFlush) >= m_period)
    {
        //TPS Report
        size_t tps = 0.0;
        if(m_tickCount > 0)
            tps = m_tickCount / (now - m_lastFlush);

        std::cout << tps << " tics per second" << std::endl;

        //Reset
        m_tickCount = 0;
        m_lastFlush = now;
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
    for(size_t x = 0; x < 0xffff; ++x)
    {
        anchor = x;
    }
}

int main()
{
    time_t start = std::time(NULL);

    CRateState rateWatch(5);

    // Loop for twenty seconds
    for(time_t latest = start; (latest - start) < 20; latest = std::time(NULL))
    {
        // Do something.
        something_we_do();

        // Note that we did something.
        rateWatch.Tick();
    }

    return 0;
}
