defmodule HundredDoors do

  def doors() do
     Enum.to_list(Stream.take(Stream.cycle([false]), 100))
  end

  def toggle(doors, n) do
     Enum.take(doors, n) ++ [not Enum.at(doors,n)] ++ Enum.drop(doors,n+1)
  end

  def toggle_every(doors, n) do
    Enum.reduce( Enum.take_every((n-1)..99, n), doors, fn(n, acc) -> toggle(acc, n) end )
  end

end

# unoptimized
final_state = Enum.reduce(1..100, HundredDoors.doors, fn(n, acc) -> HundredDoors.toggle_every(acc, n) end)

# optimized
final_state = Enum.reduce(1..10, HundredDoors.doors, fn(n, acc) -> HundredDoors.toggle(acc, n*n-1) end)

open_doors = Enum.map(Enum.filter( Enum.with_index(final_state), fn({door,_}) -> door end ),
                      fn({_,index}) -> index+1 end)

IO.puts "All doors are closed except these: #{inspect open_doors}"
