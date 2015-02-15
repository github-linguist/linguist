declare
  fun lazy {HammingFun}
     1|{FoldL1 [{MultHamming 2} {MultHamming 3} {MultHamming 5}] LMerge}
  end

  Hamming = {HammingFun}

  fun {MultHamming N}
     {LMap Hamming fun {$ X} N*X end}
  end

  fun lazy {LMap Xs F}
     case Xs
     of nil  then nil
     [] X|Xr then {F X}|{LMap Xr F}
     end
  end

  fun lazy {LMerge Xs=X|Xr Ys=Y|Yr}
     if     X < Y then X|{LMerge Xr Ys}
     elseif X > Y then Y|{LMerge Xs Yr}
     else              X|{LMerge Xr Yr}
     end
  end

  fun {FoldL1 X|Xr F}
     {FoldL Xr F X}
  end
in
  {ForAll {List.take Hamming 20} System.showInfo}
  {System.showInfo {Nth Hamming 1690}}
  {System.showInfo {Nth Hamming 1000000}}
