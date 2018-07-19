require 'optparse'
require 'prime'

maximum = 10
OptionParser.new do |o|
  o.banner = "Usage: #{File.basename $0} [-m MAXIMUM]"
  o.on("-m MAXIMUM", Integer,
       "Count up to MAXIMUM [#{maximum}]") { |m| maximum = m }
  o.parse! rescue ($stderr.puts $!, o; exit 1)
  ($stderr.puts o; exit 1) unless ARGV.size == 0
end

# 1 has no prime factors
puts "1 is 1" unless maximum < 1

2.upto(maximum) do |i|
  # i is 504 => i.prime_division is [[2, 3], [3, 2], [7, 1]]
  f = i.prime_division.map! do |factor, exponent|
    # convert [2, 3] to "2 x 2 x 2"
    ([factor] * exponent).join " x "
  end.join " x "
  puts "#{i} is #{f}"
end
