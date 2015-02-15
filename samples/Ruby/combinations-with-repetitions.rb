possible_doughnuts = ['iced', 'jam', 'plain'].repeated_combination(2)
puts "There are #{possible_doughnuts.count} possible doughnuts:"
possible_doughnuts.each{|doughnut_combi| puts doughnut_combi.join(' and ')}
