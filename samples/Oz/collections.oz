declare
  %% Lists (immutable, recursive)
  Xs = [1 2 3 4]
  %% Add element at the front (cons)
  Xs0 = 0|Xs
  {Show {Length Xs}} %% output: 4

  %% Records (immutable maps with a label)
  Rec = label(1:2 symbol:3)
  {Show Rec}   %% output: label(2 symbol:3)
  {Show Rec.1} %% output: 2
  %% create a new record with an added field
  Rec2 = {AdjoinAt Rec 2 value}
  {Show Rec2}  %% output: label(2 value symbol:3)

  %% Dictionaries (mutable maps)
  Dict = {Dictionary.new}
  Dict.1 := 1
  Dict.symbol := 3
  {Show Dict.1} %% output: 1

  %% Arrays (mutable with integer keys)
  Arr = {Array.new 1 10 initValue}
  Arr.1 := 3
  {Show Arr.1} %% output: 3
