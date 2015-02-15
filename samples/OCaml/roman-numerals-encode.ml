let digit x y z = function
    1 -> [x]
  | 2 -> [x;x]
  | 3 -> [x;x;x]
  | 4 -> [x;y]
  | 5 -> [y]
  | 6 -> [y;x]
  | 7 -> [y;x;x]
  | 8 -> [y;x;x;x]
  | 9 -> [x;z]

let rec to_roman x =
  if x = 0 then []
  else if x < 0 then
    invalid_arg "Negative roman numeral"
  else if x >= 1000 then
    'M' :: to_roman (x - 1000)
  else if x >= 100 then
    digit 'C' 'D' 'M' (x / 100) @ to_roman (x mod 100)
  else if x >= 10 then
    digit 'X' 'L' 'C' (x / 10) @ to_roman (x mod 10)
  else
    digit 'I' 'V' 'X' x
