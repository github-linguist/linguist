declare
  %% returns milliseconds
  fun {TimeIt Proc}
     Before = {Now}
  in
     {Proc}
     {Now} - Before
  end

  fun {Now}
     {Property.get 'time.total'}
  end
in
  {Show
   {TimeIt
    proc {$}
       {FoldL {List.number 1 1000000 1} Number.'+' 4 _}
    end}
  }
