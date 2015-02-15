def multifact(n, d)
  n.step(1, -d).inject( :* )
end

(1..5).each {|d| puts "Degree #{d}: #{(1..10).map{|n| multifact(n, d)}.join "\t"}"}
