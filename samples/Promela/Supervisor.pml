#include "./Thread.pml"

/**
 * Supervisor propagates commands in serial mode if and only if this variable is
 * set to true. As of ADAPRO 5.0.0, Supervisor keeps querying a configuration
 * parameter corresponding with this condition during runtime.
 */
bool serialize_commands;

/**
 * Should be true if and only if all Workers have entered the state STOPPED.
 * This is a private field of Supervisor, but modeled as a global variable to
 * enable more precise simulation of constructors.
 */
bool all_workers_stopped;

/**
 * Should be true if and only if one or more Workers have entered the state
 * ABORTED. This is a private field of Supervisor, but modeled as a global
 * variable to enable more precise simulation of constructors.
 */
bool exists_aborted_worker;

/**
 * Supervisor sets this true when calling its transition callback on CONTINUE
 * for the first time. This is used for the mechanism that prevents Supervisor
 * for propagating the CONTINUE command when starting. Doing so would prevent
 * Workers from starting in the state PAUSED. This is a private field of
 * Supervisor, but modeled as a global variable to enable more precise
 * simulation of constructors.
 */
bool supervisor_started;

/**
 * Asserts that all the threads from k to N are in the given state with the
 * given command. Mutates the value of k, leaving it 0 at the end.
 */
inline assert_all(k, state, command)
{
    d_step
    {
        do
        ::  k < N   ->
                assert(get_state(k)     == state);
                assert(get_command(k)   == command);
                k++
        ::  else    ->
                break
        od
        k = 0
    }
}

/** Sends the given command to Worker number j. Waits iff wait == true. */
inline send_command(j, command, wait)
{
    if
    ::  command == START                    ->
            start(j, wait)
    ::  command == PAUSE                    ->
            pause(j, wait)
    ::  command == CONTINUE                 ->
            resume(j, wait)
    ::  command == STOP || command == ABORT ->
            stop(j, wait)
    fi
}

/**
 * Propagates the given command in covariant order, i.e. by iterating through
 * Workers in increasing order. Blocks if and only if wait is true.
 */
inline covariant_propagation(command, wait)
{
    do
    ::  j < N   ->
            send_command(j, command, wait);
            j++
    ::  else    ->
            j = 1;
            break
    od
}

/**
 * Propagates the given command in contravariant order, i.e. by iterating
 * through Workers in decreasing order.
 */
inline contravariant_propagation(command)
{
    j = N - 1;
    do
    ::  j > 0   ->
            send_command(j, command, true);
            j--
    ::  else    ->
            j = 1;
            break
    od
}

/**
 * Propagates the given command to all Workers, using the command propagate mode
 * determined by the value of the global variable serialize_commands.
 */
inline propagate_command(command)
{
    if
    ::  command == CONTINUE && !supervisor_started ->
            printf("Supervisor doesn't propagate the first CONTINUE command.\n");
            supervisor_started = true
    ::  else ->
            if
            ::  serialize_commands  -> propagate_command_in_lifo(command)
            ::  else                -> propagate_command_in_parallel(command)
            fi
    fi
}

/**
  * Sends the given command to all Workers using serial command propagation
  * mode. This means sending every command in the blocking way.
  */
inline propagate_command_in_lifo(command)
{
    if
    ::  command == START || command == PAUSED   ->
            covariant_propagation(command, true)
    ::  else                                    ->
            contravariant_propagation(command)
    fi
}

/**
  * Sends the given command to all Workers using parallel command propagation
  * mode. This means sending commands asynchronously first to every Worker, and
  * then waiting for them move to an appropriate state.
  */
inline propagate_command_in_parallel(command)
{
    printf("Supervisor propagating command %e...\n", command);
    covariant_propagation(command, false);
    do
    ::  j < N  ->
            if
            ::  command == START    -> wait_for_START_mask(j)
            ::  command == CONTINUE -> wait_for_RESUME_mask(j)
            ::  command == PAUSE    -> wait_for_PAUSE_mask(j)
            ::  command == STOP     -> wait_for_HALT_mask(j)
            fi
            j++
    ::  else   ->
            j = 1;
            break
    od
}

/**
 * Checks whether of not there all Workers have moved to the state STOPPED or if
 * there exists a worker in state ABORTED and sets the appropriate booleans.
 */
inline check_worker_states()
{
    all_workers_stopped = true;
    do
    ::  j < N   ->
            all_workers_stopped     =
                all_workers_stopped && get_state(j) == STOPPED;
            exists_aborted_worker   =
                exists_aborted_worker || get_state(j) == ABORTED;
            j++
    ::  else    ->
            j = 1;
            break
    od
}

inline sv_start_sync()
{
    set_command(0, START);
    run Supervisor();
    wait_for_START_mask(0);
}

/**
 * Models the method ADAPRO::Control::Supervisor::prepare.
 */
inline sv_prepare()
{
    propagate_command(START);
}

/**
 * Models the method ADAPRO::Control::Supervisor::execute.
 */
inline sv_execute()
{
    executing[0] = true;
    check_worker_states();
    if
    ::  all_workers_stopped     ->
            printf("Supervisor will stop because all Workers have stopped...\n");
            stop(0, false)
    ::  exists_aborted_worker   ->
            printf("Supervisor will stop because some Workers have aborted...\n");
            stop(0, false)
    ::  else                    ->
            skip
    fi
    executing[0] = false
}

/** Models the transition callback of ADAPRO::Control::Supervisor. */
inline sv_trans_cb(s)
{
    if
    ::  s == RUNNING                    ->
            propagate_command(CONTINUE)
    ::  s == PAUSED                     ->
            propagate_command(PAUSE)
    ::  s == ABORTING || s == STOPPING  ->
            propagate_command(STOP)
    ::  else                            ->
            skip
    fi
}

inline sv_covariant_transition(state, command, next)
{
    print_state_transition(0, state, command, next);
    set_state(0, next);
    sv_trans_cb(next)
}

inline sv_contravariant_transition(state, command, next)
{
    sv_trans_cb(next);
    print_state_transition(0, state, command, next);
    set_state(0, next)
}

/**
 * Models the constructor of Supervisor, which first sets its own state and
 * command and then calls the Worker constructors.
 */
inline sv_ctor(i)
{
    d_step
    {
        if
        :: true -> serialize_commands = true
        :: true -> serialize_commands = false
        fi
        all_workers_stopped     = false;
        exists_aborted_worker   = false;
        supervisor_started      = false;
        do
        ::  i < N   ->
                ctor(i);
                i++
        ::  else    ->
                break
        od
    }
}

/**
 * Models the destructor of Supervisor.
 */
inline sv_dtor(i)
{
    d_step
    {
        do
        ::  i < N   ->
                dtor(i);
                i++
        ::  else    ->
                break
        od
    }
}

/** Models the ADAPRO Supervisor Thread. */
proctype Supervisor()
{
    /**
     * The variable j is a local index variable that Supervisor uses for
     * iterating over Workers. Supervisor itself is the Thread number 0, so
     * this variable needs to always stay in the open interval ]0,N[.
     */
    byte j = 1;

    /* Startup: */
    get_command(0) == START;
    sv_covariant_transition(READY, START, STARTING);
    set_command(0, CONTINUE);
    sv_prepare();

    /* Execution: */
    do
    ::  get_command(0) == CONTINUE                          ->
            assert(get_state(0) == STARTING || get_state(0) == RUNNING ||
                get_state(0) == PAUSED);
            if
            ::  get_state(0) == STARTING    ->
                    sv_covariant_transition(STARTING, CONTINUE, RUNNING)
            ::  get_state(0) == PAUSED      ->
                    sv_covariant_transition(PAUSED, CONTINUE, RUNNING)
            ::  else                        ->
                    skip
            fi
            sv_execute()
    ::  get_command(0) == PAUSE                             ->
            if
            ::  get_state(0) == PAUSED      ->
                    get_command(0) != PAUSED
            ::  else                        ->
                    assert(get_state(0) == STARTING || get_state(0) == RUNNING ||
                        get_state(0) == STARTING);
                    if
                    ::  get_state(0) == STARTING || get_state(0) == RUNNING ->
                            sv_contravariant_transition(RUNNING, PAUSE, PAUSED)
                    ::  else                                                ->
                            skip
                    fi
            fi
    ::  get_command(0) == STOP || get_command(0) == ABORT   ->
            break
    od

    /* Shutdown: */
    if
    ::  get_command(0) == STOP      ->
            assert(get_state(0) == STARTING || get_state(0) == RUNNING ||
                get_state(0) == PAUSED);
            sv_covariant_transition(get_state(0), STOP, STOPPING);
            sv_contravariant_transition(STOPPING, STOP, STOPPED)
    ::  get_command(0) == ABORT     ->
            assert(get_state(0) == STARTING || get_state(0) == RUNNING ||
                get_state(0) == STOPPING);
            sv_covariant_transition(get_state(0), ABORT, ABORTING);
            sv_contravariant_transition(ABORTING, ABORT, ABORTED)
    ::  else                        ->
            assert(false)
    fi
}
