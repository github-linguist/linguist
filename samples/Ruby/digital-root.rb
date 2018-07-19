class String
  def digroot_persistence(base)
    num = self.to_i(base)
    persistence = 0
    until num < base do
      num = num.to_s(base).each_char.reduce(0){|m, c| m += c.to_i(base) }
      persistence += 1
    end
    [num.to_s(base), persistence]
  end
end

#Handles bases upto 36; Demo:
[["101101110110110010011011111110011000001", 2],
 [ "5BB64DFCC1", 16],
 ["5", 10],
 ["393900588225", 10],
 ["50YE8N29", 36]].each{|(str, base)| puts "#{str} base #{base} has a digital root \
of %s and a persistence of %s." % str.digroot_persistence(base) }
