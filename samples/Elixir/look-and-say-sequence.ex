defmodule LookAndSay do
  def next(n) do
    Enum.chunks_by(to_char_list(n), &(&1))
    |> Enum.map(fn cl=[h|_] ->
                Enum.concat(to_char_list(length cl), [h]) end)
    |> Enum.concat
    |> list_to_integer
  end

  def sequence_from(n) do
    Stream.iterate n, &(LookAndSay.next/1)
  end

  def main([start_str|_]) do
    {start_val,_} = Integer.parse(start_str)
    :io.format("~w~n", [LookAndSay.sequence_from(start_val) |> Enum.take 9] )
    exit 0
  end

  def main([]) do
    main(["1"])
  end
end

LookAndSay.main(System.argv)
