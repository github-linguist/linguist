declare

  fun {Ack M N}
     if     M == 0 then N+1
     elseif N == 0 then {Ack M-1 1}
     else               {Ack M-1 {Ack M N-1}}
     end
  end

in

  {Show {Ack 3 7}}
