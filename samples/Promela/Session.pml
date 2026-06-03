#include "Supervisor.pml"
/* Supervisor.pml includes Thread.pml (which includes Invariants.prp) */

init
{
    byte i = 0;
    assert_all(i, 0, 0);
    sv_ctor(i);
    i = 0;
    assert_all(i, READY, CONTINUE);
    printf("Init starts Supervisor...\n");
    sv_start_sync();
    do
    ::  if
        ::  get_state(0) == RUNNING ->
                printf("Init pauses Supervisor...\n");
                if
                ::  true -> pause(0, true)
                ::  true -> pause(0, false)
                fi
        ::  get_state(0) == PAUSED  ->
                printf("Init resumes Supervisor...\n");
                if
                ::  true -> resume(0, true)
                ::  true -> resume(0, false)
                fi
        ::  true -> skip
        fi
    ::  break
    od
    if
    ::  printf("Init stops Supervisor...\n");
        if
        ::  true -> stop(0, true)
        ::  true -> stop(0, false)
        fi
    ::  true -> skip
    fi
    wait_for_HALT_mask(0);
    i = 0;
    sv_dtor(i);
    i = 0;
    assert_all(i, 0, 0)
}
