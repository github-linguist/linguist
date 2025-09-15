#include "./Theory.prp"
/* TODO: Rename Thread to Worker, as Supervisor has its own Promela file. */

/** The number of Threads, including Supervisor as Thread no. 0, to simulate. */
#define N 3

/**
 * The numerical values of the states are:
 *     ABORTED     = 1,
 *     ABORTING    = 2,
 *     STOPPED     = 3,
 *     STOPPING    = 4,
 *     PAUSED      = 5,
 *     RUNNING     = 6,
 *     STARTING    = 7,
 *     READY       = 8
 */
mtype = {READY, STARTING, RUNNING, PAUSED, STOPPING, STOPPED, ABORTING, ABORTED};

/**
 * The numerical values of the commands are:
 *     ABORT       = 9,
 *     STOP        = 10,
 *     PAUSE       = 11,
 *     CONTINUE    = 12,
 *     START       = 13
 */
mtype = {START, CONTINUE, PAUSE, STOP, ABORT};

/**
 * It seems that mtype declarations have "big endian" ordering, which I find
 * counterintuitive. Hence, this definition:
 */
#define LT(x, y) x > y

/**
 * Prettyprints the state transition for Thread number i. For automatic state
 * transitions, the command should be set to 0, in which case an underscore
 * ('_') will be printed instead of a command name.
 */
inline print_state_transition(i, state, command, next)
{
    if
    ::  i == 0  && command == 0 ->
            printf("Supervisor: (%e, _) -> %e\n", state, next)
    ::  i == 0  && command > 0  ->
            printf("Supervisor: (%e, %e) -> %e\n", state, command, next)
    ::  i > 0   && command == 0 ->
            printf("Worker %d: (%e, _) -> %e\n", i, state, next)
    ::  i > 0   && command > 0 ->
            printf("Worker %d: (%e, %e) -> %e\n", i, state, command, next)
    fi
}

/********************************** PRIVATE ***********************************/

/** States of the Threads. Index 0 is reserved for the Supervisor. */
mtype states[N];

/** Commands of the Threads. Index 0 is reserved for the Supervisor. */
mtype commands[N];

/** The array of Threads currently executing. */
bool executing[N];

/** See the documentation for ADAPRO::Control::Thread::set_state. */
#define set_state(i, state) states[i] = state

/** See the documentation for ADAPRO::Control::Thread::set_command. */
#define set_command(i, command)                                                \
    atomic                                                                     \
    {                                                                          \
        if                                                                     \
        ::  command == START && commands[i] == CONTINUE ->                     \
                assert(get_state(i) == READY);                                 \
                commands[i] = START                                            \
        ::  command == PAUSE && commands[i] == CONTINUE ->                     \
                assert(get_state(i) == STARTING || get_state(i) == RUNNING);   \
                commands[i] = PAUSE                                            \
        ::  command == CONTINUE &&                                             \
            (commands[i] == START || commands[i] == PAUSE) ->                  \
                assert(get_state(i) == STARTING || get_state(i) == PAUSED);    \
                commands[i] = CONTINUE                                         \
        ::  command == STOP &&                                                 \
            (commands[i] == CONTINUE || commands[i] == PAUSE) ->               \
                assert(get_state(i) == STARTING || get_state(i) == RUNNING ||  \
                        get_state(i) == PAUSED);                               \
                commands[i] = STOP                                             \
        ::  command == ABORT ->                                                \
                assert(get_state(i) != READY);                                 \
                commands[i] = ABORT                                            \
        ::  else -> printf("Thread %d ignores command %e.\n", i, commands[i])  \
        fi                                                                     \
    }

/********************************** PUBLIC ************************************/

/** See the documentation for ADAPRO::Control::Thread::get_state. */
#define get_state(i) states[i]

/** See the documentation for ADAPRO::Control::Thread::get_command. */
#define get_command(i) commands[i]

/********************************* PROTECTED **********************************/

/**
 * Models the method ADAPRO::Control::Thread::prepare by non-deterministically
 * choosing between aborting, pausing, and doing nothing.
 */
inline prepare(i)
{
    if
    :: true -> abort(i)
    :: true -> set_command(i, PAUSE)
    :: true -> set_command(i, STOP)
    :: true -> skip
    fi
}

/**
 * Models the method ADAPRO::Control::Thread::execute by non-deterministically
 * choosing between aborting, pausing, stopping, and doing nothing.
 */
inline execute(i)
{
    executing[i] = true;
    if
    :: true -> abort(i)
    /* :: true -> set_command(i, PAUSE) */
    :: true -> set_command(i, STOP)
    :: true -> skip
    fi
    executing[i] = false
}

/**
 * Models the method ADAPRO::Control::Thread::finish by non-deterministically
 * choosing between aborting and doing nothing.
 */
inline finish(i)
{
    if
    :: true -> abort(i)
    :: true -> skip
    fi
}

/** Aborts the Thread number i. Always non-blocking. */
inline abort(i)
{
    assert(_pid != 1);      /* Supervisor is not allowed to abort Workers. */
    set_command(i, ABORT);
}

/********************************** PRIVATE ***********************************/

/**
 * Simulates the Thread class constructor by initializing the state and
 * commmand of thread number i Thread with their legal default values.
 */
inline ctor(i)
{
    d_step
    {
        states[i]   = READY;
        commands[i] = CONTINUE
    }
}

inline dtor(i)
{
    d_step
    {
        states[i]   = 0;
        commands[i] = 0;
        executing[i]= false
    }
}

/** Models the default transition callback of a Thread that does nothing. */
inline trans_cb(s)
{
    skip
}

inline covariant_transition(state, command, next)
{
    print_state_transition(k, state, command, next);
    set_state(k, next);
    /*trans_cb(next)*/
}

inline contravariant_transition(state, command, next)
{
    /*trans_cb(next);*/
    print_state_transition(k, state, command, next);
    set_state(k, next)
}

/** Models the ADAPRO::Control::Thread::run method. */
proctype Thread(byte k)
{
    /* Startup: */
    get_command(k) == START;
    covariant_transition(READY, START, STARTING);
    set_command(k, CONTINUE);
    prepare(k);

    /* Execution: */
    do
    ::  get_command(k) == CONTINUE                          ->
            assert(get_state(k) == STARTING || get_state(k) == RUNNING ||
                get_state(k) == PAUSED);
            if
            ::  get_state(k) == STARTING    ->
                    covariant_transition(STARTING, CONTINUE, RUNNING)
            ::  get_state(k) == PAUSED      ->
                    covariant_transition(PAUSED, CONTINUE, RUNNING)
            ::  else                        ->
                    skip
            fi
            execute(k)
    ::  get_command(k) == PAUSE                             ->
            if
            ::  get_state(k) == PAUSED  ->
                    get_command(k) != PAUSED
            ::  else                    ->
                    assert(get_state(k) == STARTING || get_state(k) == RUNNING ||
                        get_state(k) == STARTING);
                    if
                    ::  get_state(k) == STARTING || get_state(k) == RUNNING ->
                            contravariant_transition(RUNNING, PAUSE, PAUSED)
                    ::  else                    ->
                            skip
                    fi
            fi
    ::  get_command(k) == STOP || get_command(k) == ABORT   ->
            break
    od

    /* Shutdown: */
    if
    ::  get_command(k) == STOP      ->
            assert(get_state(k) == STARTING || get_state(k) == RUNNING ||
                get_state(k) == PAUSED);
            covariant_transition(get_state(k), STOP, STOPPING);
            finish(k);
            contravariant_transition(STOPPING, STOP, STOPPED)
    ::  get_command(k) == ABORT     ->
            assert(get_state(k) == STARTING || get_state(k) == RUNNING ||
                get_state(k) == STOPPING);
            covariant_transition(get_state(k), ABORT, ABORTING);
            contravariant_transition(ABORTING, ABORT, ABORTED)
    ::  else                        ->
            assert(false)
    fi
}

/********************************** PUBLIC ************************************/

/** Models Control::Thread::wait_for_state_mask(Data::State::START_MASK). */
inline wait_for_START_mask(i)
{
    if
    ::  _pid == 0   ->
            printf("Init is waiting for START mask on Thread %d...\n", i)
    ::  _pid == 1   ->
            printf("Supervisor is waiting for START mask on Thread %d...\n", i)
    ::  _pid > 1    ->
            printf("Worker %d is waiting for START mask on Thread %d...\n", _pid - 1, i)
    fi
    get_state(i) == RUNNING     ||
    get_state(i) == PAUSED      ||
    get_state(i) == STOPPING    ||
    get_state(i) == STOPPED     ||
    get_state(i) == ABORTING    ||
    get_state(i) == ABORTED;
    printf("Waiting ended.\n")
}

/** Models Control::Thread::wait_for_state_mask(Data::State::PAUSE_MASK). */
inline wait_for_PAUSE_mask(i)
{
    if
    ::  _pid == 0   ->
            printf("Init is waiting for PAUSED mask on Thread %d...\n", i)
    ::  _pid == 1   ->
            printf("Supervisor is waiting for PAUSED mask on Thread %d...\n", i)
    ::  _pid > 1    ->
            printf("Worker %d is waiting for PAUSED mask on Thread %d...\n", _pid - 1, i)
    fi
    get_state(i) == PAUSED      ||
    get_state(i) == STOPPING    ||
    get_state(i) == STOPPED     ||
    get_state(i) == ABORTING    ||
    get_state(i) == ABORTED;
    printf("Waiting ended.\n")
}

/** Models Control::Thread::wait_for_state_mask(Data::State::RESUME_MASK). */
inline wait_for_RESUME_mask(i)
{
    if
    ::  _pid == 0   ->
            printf("Init is waiting for RESUME mask on Thread %d...\n", i)
    ::  _pid == 1   ->
            printf("Supervisor is waiting for RESUME mask on Thread %d...\n", i)
    ::  _pid > 1    ->
            printf("Worker %d is waiting for RESUME mask on Thread %d...\n", _pid - 1, i)
    fi
    get_state(i) == RUNNING     ||
    get_state(i) == STOPPING    ||
    get_state(i) == STOPPED     ||
    get_state(i) == ABORTING    ||
    get_state(i) == ABORTED;
    printf("Waiting ended.\n")
}

/** Models Control::Thread::wait_for_state_mask(Data::State::HALT_MASK). */
inline wait_for_HALT_mask(i)
{
    if
    ::  _pid == 0   ->
            printf("Init is waiting for HALT mask on Thread %d...\n", i)
    ::  _pid == 1   ->
            printf("Supervisor is waiting for HALT mask on Thread %d...\n", i)
    ::  _pid > 1    ->
            printf("Worker %d is waiting for HALT mask on Thread %d...\n", _pid - 1, i)
    fi
    get_state(i) == STOPPED     ||
    get_state(i) == ABORTED;
    printf("Waiting ended.\n")
}

/** Models Control::Thread::wait_for_state_mask(Data::State::ABORT_MASK). */
inline wait_for_ABORT_mask(i)
{
    if
    ::  _pid == 0   ->
            printf("Init is waiting for ABORTED mask on Thread %d...\n", i)
    ::  _pid == 1   ->
            printf("Supervisor is waiting for ABORTED mask on Thread %d...\n", i)
    ::  _pid > 1    ->
            printf("Worker %d is waiting for ABORTED mask on Thread %d...\n", _pid - 1, i)
    fi
    get_state(i) == ABORTED;
    printf("Waiting ended.\n")
}

/**
 * Starts the Thread number i. If wait is true, then this macro blocks until
 * the Thread has entered a state greater than or equal to RUNNING.
 */
inline start(i, wait)
{
    set_command(i, START);
    run Thread(i);
    if
    ::  wait -> wait_for_START_mask(i)
    ::  else -> skip
    fi
}

/**
 * Pauses the Thread number i. If wait is true, then this macro blocks until the
 * Thread has entered a state greater than or equal to PAUSED.
 */
inline pause(i, wait)
{
    set_command(i, PAUSE);
    if
    ::  wait -> wait_for_PAUSE_mask(i)
    ::  else -> skip
    fi
}

/**
 * Resumes the Thread number i. If wait is true, then this macro blocks until
 * the Thread has entered a state greater than or equal to RUNNING.
 */
inline resume(i, wait)
{
    set_command(i, CONTINUE);
    if
    ::  wait -> wait_for_RESUME_mask(i)
    ::  else -> skip
    fi
}

/**
 * Stops the Thread number i. If wait is true, then this macro blocks until
 * the Thread has entered the state STOPPED or ABORTED.
 */
inline stop(i, wait)
{
    set_command(i, STOP);
    if
    ::  wait -> wait_for_HALT_mask(i)
    ::  else -> skip
    fi
}
