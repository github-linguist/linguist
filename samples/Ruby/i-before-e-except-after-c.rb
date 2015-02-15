require 'open-uri'

plausibility_ratio = 2
counter = Hash.new(0)
path = 'http://www.puzzlers.org/pub/wordlists/unixdict.txt'
rules = [['I before E when not preceded by C:', 'ie', 'ei'],
         ['E before I when preceded by C:', 'cei', 'cie']]

open(path){|f| f.each{|line| line.scan(/ie|ei|cie|cei/){|match| counter[match] += 1 }}}

overall_plausible = rules.all? do |(str, x, y)|
  num_x, num_y, ratio = counter[x], counter[y], counter[x] / counter[y].to_f
  plausibility = ratio > plausibility_ratio
  puts str
  puts "#{x}: #{num_x}; #{y}: #{num_y}; Ratio: #{ratio.round(2)}: #{ plausibility ? 'Plausible' : 'Implausible'}"
  plausibility
end

puts "Overall: #{overall_plausible ? 'Plausible' : 'Implausible'}."
