declare
  fun {Twice Function X}
     {Function {Function X}}
  end
in
  {Show {Twice Sqrt 81.0}}  %% prints 3.0
