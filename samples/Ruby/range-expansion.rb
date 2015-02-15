def range_expand(rng)
  rng.split(',').collect do |part|
    if part =~ /^(-?\d+)-(-?\d+)$/
      ($1.to_i .. $2.to_i).to_a
    else
      Integer(part)
    end
  end.flatten
end

p range_expand('-6,-3--1,3-5,7-11,14,15,17-20')
