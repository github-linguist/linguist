declare
  fun {MaxLicenses Filename ?Times}
     InUse = {NewCell 0}
     MaxInUse = {NewCell 0}
     MaxTimes = {NewCell nil}
  in
     for Job in {ReadLines Filename} do
        case {List.take Job 11} of "License OUT" then
  	 InUse := @InUse + 1
  	 if @InUse > @MaxInUse then
  	    MaxInUse := @InUse
  	    MaxTimes := nil
  	 end
  	 if @InUse == @MaxInUse then
	    JobTime = {Nth {String.tokens Job & } 4}
	 in
  	    MaxTimes := JobTime|@MaxTimes
  	 end
        [] "License IN " then
  	 InUse := @InUse - 1
        end
     end
     Times = {Reverse @MaxTimes}
     @MaxInUse
  end

  %% Helper.
  %% Returns a lazy list. So we don't keep the whole logfile in memory...
  fun {ReadLines Filename}
     F = {New class $ from Open.file Open.text end init(name:Filename)}
     fun lazy {ReadNext}
        case {F getS($)} of
           false then nil
        [] Line then
           Line|{ReadNext}
        end
     end
  in
     %% close file when handle becomes unreachable
     {Finalize.register F proc {$ F} {F close} end}
     {ReadNext}
  end

  Times
  MaxInUse = {MaxLicenses "mlijobs.txt" ?Times}
in
  {System.showInfo
   "Maximum simultaneous license use is "#MaxInUse#" at the following times:"}
  {ForAll Times System.showInfo}
