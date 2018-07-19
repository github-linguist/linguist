let rec forward_difference = function
    a :: (b :: _ as xs) ->
      b - a :: forward_difference xs
  | _ ->
      []

let rec nth_forward_difference n xs =
  if n = 0 then
    xs
  else
    nth_forward_difference (pred n) (forward_difference xs)
