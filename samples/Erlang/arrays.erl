  %% Create a fixed-size array with entries 0-9 set to 'undefined'
  A0 = array:new(10).
  10 = array:size(A0).

  %% Create an extendible array and set entry 17 to 'true',
  %% causing the array to grow automatically
  A1 = array:set(17, true, array:new()).
  18 = array:size(A1).

  %% Read back a stored value
  true = array:get(17, A1).

  %% Accessing an unset entry returns the default value
  undefined = array:get(3, A1).

  %% Accessing an entry beyond the last set entry also returns the
  %% default value, if the array does not have fixed size
  undefined = array:get(18, A1).

  %% "sparse" functions ignore default-valued entries
  A2 = array:set(4, false, A1).
  [{4, false}, {17, true}] = array:sparse_to_orddict(A2).

  %% An extendible array can be made fixed-size later
  A3 = array:fix(A2).

  %% A fixed-size array does not grow automatically and does not
  %% allow accesses beyond the last set entry
  {'EXIT',{badarg,_}} = (catch array:set(18, true, A3)).
  {'EXIT',{badarg,_}} = (catch array:get(18, A3)).
