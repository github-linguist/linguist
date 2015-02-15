declare
  proc {PrintNumber N}
     N=42  %% assert
     {Show N}
  end
in
  {PrintNumber 42} %% ok
  {PrintNumber 11} %% throws
