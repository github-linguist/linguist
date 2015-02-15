declare
  fun {F N}
     if N == 0 then 1
     elseif N > 0 then N - {M {F N-1}}
     end
  end

  fun {M N}
     if N == 0 then 0
     elseif N > 0 then N - {F {M N-1}}
     end
  end
in
  {Show {Map {List.number 0 9 1} F}}
  {Show {Map {List.number 0 9 1} M}}
