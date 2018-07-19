declare
  Philosophers = [aristotle kant spinoza marx russell]

  proc {Start}
     Forks = {MakeList {Length Philosophers}}
  in
     {ForAll Forks NewFork}
     for
        Name in Philosophers
        LeftFork in Forks
        RightFork in {RightShift Forks}
     do
        thread
           {Philosopher Name LeftFork RightFork}
        end
     end
  end

  proc {Philosopher Name LeftFork RightFork}
     for do
        {ShowInfo Name#" is hungry."}

        {TakeForks [LeftFork RightFork]}
        {ShowInfo Name#" got forks."}
        {WaitRandom}
        {ReleaseFork LeftFork}
        {ReleaseFork RightFork}

        {ShowInfo Name#" is thinking."}
        {WaitRandom}
     end
  end

  proc {WaitRandom}
     {Delay 1000 + {OS.rand} mod 4000} %% 1-5 seconds
  end

  proc {TakeForks Forks}
     {ForAll Forks WaitForFork}
     case {TryAtomically proc {$}
                            {ForAll Forks TakeFork}
                         end}
     of true then
        {ForAll Forks InitForkNotifier}
     [] false then
        {TakeForks Forks}
     end
  end

  %%
  %% Fork type
  %%

  %% A fork is a mutable reference to a pair
  fun {NewFork}
     {NewCell
      unit(taken:_     %% a fork is taken by setting this value to a unique value
           notify:unit %% to wait for a taken fork
	  )}
  end

  proc {TakeFork F}
     (@F).taken = {NewName}
  end

  proc {InitForkNotifier F}
     %% we cannot do this in TakeFork
     %% because side effect are not allowed in subordinate spaces
     New Old
  in
     {Exchange F Old New}
     New = unit(taken:Old.taken notify:_)
  end

  proc {ReleaseFork F}
     New Old
  in
     {Exchange F Old New}
     New = unit(taken:_ notify:unit)
     Old.notify = unit %% notify waiters
  end

  proc {WaitForFork F}
     {Wait (@F).notify}  %% returns immediatly if fork is free, otherwise blocks
  end

  %%
  %% Helpers
  %%

  %% Implements transactions on data flow variables
  %% with computation spaces. Returns success.
  fun {TryAtomically P}
     try
	S = {Space.new
	     proc {$ Sync}
		{P}
		Sync = unit
	     end}
     in
	{Space.askVerbose S} \= failed = true
	{Wait {Space.merge S}}
	true
     catch _ then
	false
     end
  end

  fun {RightShift Xs} %% circular
     case Xs of nil then nil
     else {Append Xs.2 [Xs.1]}
     end
  end

  ShowInfo = System.showInfo
in
  {Start}
