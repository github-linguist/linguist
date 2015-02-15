declare
  fun{Square A}
    A*A
  end

  Lst = [1 2 3 4 5]

  %% apply a PROCEDURE to every element
  {ForAll Lst Show}

  %% apply a FUNCTION to every element
  Result = {Map Lst Square}
  {Show Result}
