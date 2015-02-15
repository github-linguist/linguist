Enum.each 1..100, fn x ->
  IO.puts(case { rem(x, 5) == 0, rem(x,3) == 0 } do
    { true, true } ->
      "FizzBuzz"
    { true, false } ->
      "Fizz"
    { false, true } ->
      "Buzz"
    { false, false } ->
      x
  end)
end
