declare
  fun {Repeat Xs N}
     if N > 0 then
        {Append Xs {Repeat Xs N-1}}
     else
        nil
     end
  end
in
  {System.showInfo {Repeat "Ha" 5}}
