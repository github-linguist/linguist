declare
  fun {LCS Xs Ys}
     case [Xs Ys]
     of [nil _]                   then nil
     [] [_ nil]                   then nil
     [] [X|Xr  Y|Yr] andthen X==Y then X|{LCS Xr Yr}
     [] [_|Xr  _|Yr]              then {Longest {LCS Xs Yr} {LCS Xr Ys}}
     end
  end

  fun {Longest Xs Ys}
     if {Length Xs} > {Length Ys} then Xs else Ys end
  end
in
  {System.showInfo {LCS "thisisatest" "testing123testing"}}
