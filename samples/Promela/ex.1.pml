mtype = { Wakeme, Running }

bit	lk, sleep_q
bit	r_lock, r_want
mtype State = Running

active proctype client() {
    sleep:					            // sleep routine
        atomic { (lk == 0) -> lk = 1 }	// spinlock(&lk)
        do				                // while r->lock
            :: (r_lock == 1) ->		    // r->lock == 1
                r_want = 1
                State = Wakeme
                lk = 0			        // freelock(&lk)
                (State == Running)	    // wait for wakeup
            :: else ->			        // r->lock == 0
                break
        od;
    progress:
        assert(r_lock == 0)		// should still be true
        r_lock = 1			    // consumed resource
        lk = 0				    // freelock(&lk)
        goto sleep
}

active proctype server() {		// interrupt routine
  wakeup:					    // wakeup routine
      r_lock = 0			    // r->lock = 0
      (lk == 0)			        // waitlock(&lk)
      if
        :: r_want ->			// someone is sleeping
            atomic {		    // spinlock on sleep queue
                (sleep_q == 0) -> sleep_q = 1
            }
            r_want = 0

            #ifdef PROPOSED_FIX
                (lk == 0)	    // waitlock(&lk)
            #endif
            if
            :: (State == Wakeme) ->
                State = Running
            :: else ->
            fi;
            sleep_q = 0
        :: else ->
      fi
      goto wakeup
  }
