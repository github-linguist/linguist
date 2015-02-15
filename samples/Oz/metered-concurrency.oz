declare
  fun {NewSemaphore N}
     sem(max:N count:{NewCell 0} 'lock':{NewLock} sync:{NewCell _})
  end

  proc {Acquire Sem=sem(max:N count:C 'lock':L sync:S)}
     Sync
     Acquired
  in
     lock L then
        if @C < N then
         C := @C + 1
         Acquired = true
        else
         Sync = @S
         Acquired = false
        end
     end
     if {Not Acquired} then
        {Wait Sync}
        {Acquire Sem}
     end
  end

  proc {Release sem(count:C 'lock':L sync:S ...)}
     lock L then
        C := @C - 1
        @S = unit %% wake up waiting threads
        S := _ %% prepare for new waiters
     end
  end

  proc {WithSemaphore Sem Proc}
     {Acquire Sem}
     try
        {Proc}
     finally
        {Release Sem}
     end
  end

  S = {NewSemaphore 4}

  proc {StartWorker Name}
     thread
	for do
	   {WithSemaphore S
	    proc {$}
	       {System.showInfo Name#" acquired semaphore"}
	       {Delay 2000}
	    end
	   }
	   {Delay 100}
	end
     end
  end
in
  for I in 1..10 do
     {StartWorker I}
  end
