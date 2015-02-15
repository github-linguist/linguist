declare
  fun {CommonPrefix Sep Paths}
     fun {GetParts P} {String.tokens P Sep} end
     Parts = {ZipN {Map Paths GetParts}}
     EqualParts = {List.takeWhile Parts fun {$ X|Xr} {All Xr {Equals X}} end}
  in
     {Join Sep {Map EqualParts Head}}
  end

  fun {ZipN Xs}
     if {Some Xs {Equals nil}} then nil
     else
        {Map Xs Head} | {ZipN {Map Xs Tail}}
     end
  end

  fun {Join Sep Xs}
     {FoldR Xs fun {$ X Z} {Append X Sep|Z} end nil}
  end

  fun {Equals C}
     fun {$ X} X == C end
  end

  fun {Head X|_} X end

  fun {Tail _|Xr} Xr end
in
  {System.showInfo {CommonPrefix &/
                    ["/home/user1/tmp/coverage/test"
                     "/home/user1/tmp/covert/operator"
                     "/home/user1/tmp/coven/members"]}}
