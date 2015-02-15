defmodule Bottles do
  def run do
    Enum.each 99..1, fn idx ->
      IO.puts "#{idx} bottle#{plural(idx)} of beer on the wall"
      IO.puts "#{idx} bottle#{plural(idx)} of beer"
      IO.puts "Take one down, pass it around"
      IO.puts "#{idx - 1} bottle#{plural(idx-1)} of beer on the wall"
      IO.puts ""
    end
  end

  def plural(1), do: ""
  def plural(num), do: "s"
end

Bottles.run
