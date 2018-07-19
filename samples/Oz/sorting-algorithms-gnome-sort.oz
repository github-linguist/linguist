declare
  fun {GnomeSort Xs}
     case Xs of nil then nil
     [] X|Xr then {Loop [X] Xr}
     end
  end

  fun {Loop Vs Ws}
     case [Vs Ws]
     of [V|_  W|Wr] andthen V =< W then {Loop W|Vs Wr}
     [] [V|Vr W|Wr] then {Loop Vr W|V|Wr}
     [] [nil  W|Wr] then {Loop [W] Wr}
     [] [Vs   nil ] then {Reverse Vs}
     end
  end
in
  {Show {GnomeSort [3 1 4 1 5 9 2 6 5]}}
