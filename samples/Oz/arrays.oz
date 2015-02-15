declare
  Arr = {Array.new 1   %% lowest index
                   10  %% highest index
                   37} %% all 10 fields initialized to 37
in
  {Show Arr.1}
  Arr.1 := 64
  {Show Arr.1}
