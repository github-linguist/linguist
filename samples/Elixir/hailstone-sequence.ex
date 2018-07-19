defmodule Hailstone do
  def step(1), do: 0
  def step(n) when Integer.even?(n), do: div(n,2)
  def step(n) when Integer.odd?(n), do: n*3 + 1
  def sequence(n) do
    Enum.to_list(Stream.take_while(Stream.iterate(n, &step/1), &(&1 > 0)))
  end

  def run do
    seq27 = Hailstone.sequence(27)
    len27 = length(seq27)
    repr = String.replace(inspect(seq27, limit: 4), "]",
                          String.replace(inspect(Enum.drop(seq27,len27-4)), "[", ", "))
    IO.puts("Hailstone(27) has #{len27} elements: #{repr}")

    {start, len}  = Enum.max_by( Enum.map(1..100_000, fn(n) -> {n, length(Hailstone.sequence(n))} end),
                                 fn({_,len}) -> len end )
    IO.puts("Longest sequence starting under 100000 begins with #{start} and has #{len} elements.")
  end
end

Hailstone.run
