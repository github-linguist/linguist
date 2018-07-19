declare
  fun {NewQueue}
     Stream
     WritePort = {Port.new Stream}
     ReadPos = {NewCell Stream}
  in
     WritePort#ReadPos
  end

  proc {Push WritePort#_ Value}
     {Port.send WritePort Value}
  end

  fun {Empty _#ReadPos}
     %% the queue is empty if the value at the current
     %% read position is not determined
     {Not {IsDet @ReadPos}}
  end

  fun {Pop _#ReadPos}
     %% blocks if empty
     case @ReadPos of X|Xr then
        ReadPos := Xr
        X
     end
  end

  Q = {NewQueue}
in
  {Show {Empty Q}}
  {Push Q 42}
  {Show {Empty Q}}
  {Show {Pop Q}}
  {Show {Empty Q}}
