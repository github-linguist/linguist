# Deal cards for FreeCell.
# http://rosettacode.org/wiki/Deal_cards_for_FreeCell

require 'optparse'

# Parse command-line arguments.
#   games = ARGV converted to Integer
#   No arguments? Pick any of first 32000 games.
games = nil
OptionParser.new do |o|
  begin
    o.banner = "Usage: #{o.program_name} number..."
    o.parse!
    games = ARGV.map {|s| Integer(s)}
    games.empty? and games = [rand(32000)]
  rescue => e
    $stderr.puts e, o
    abort
  end
end

# Define methods for old Ruby versions.
#   Enumerable#each_slice appeared in Ruby 1.8.7.
#   Enumerable#flat_map appeared in Ruby 1.9.2.
module Enumerable
  unless method_defined? :each_slice
    def each_slice(count)
      block_given? or return enum_for(:each_slice, count)
      ary = []
      each {|e|
        ary << e
        ary.length == count and (yield ary.dup; ary.clear)}
      ary.empty? or yield ary.dup
      nil
    end
  end

  unless method_defined? :flat_map
    def flat_map
      block_given? or return enum_for(:flat_map)
      ary = []
      each {|e|
        y = yield e
        ary.concat(y) rescue ary.push(y)}
      ary
    end
  end
end

# Create original deck of 52 cards, not yet shuffled.
orig_deck = %w{A 2 3 4 5 6 7 8 9 T J Q K
}.flat_map {|rank| %w{C D H S}.map {|suit| "#{rank}#{suit}"}}

games.each do |seed|
  deck = orig_deck.dup

  # Shuffle deck with random index from linear congruential
  # generator like Microsoft.
  state = seed
  52.downto(2) do |len|
    state = ((214013 * state) + 2531011) & 0x7fff_ffff
    index = (state >> 16) % len
    last = len - 1
    deck[index], deck[last] = deck[last], deck[index]
  end

  deck.reverse!  # Shuffle did reverse deck. Do reverse again.

  # Deal cards.
  puts "Game ##{seed}"
  deck.each_slice(8) {|row| puts " " + row.join(" ")}
end
