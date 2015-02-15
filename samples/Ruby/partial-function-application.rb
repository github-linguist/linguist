fs = proc { |f, s| s.map &f }
f1 = proc { |n| n * 2 }
f2 = proc { |n| n ** 2 }
fsf1 = fs.curry[f1]
fsf2 = fs.curry[f2]

[0..3, (2..8).step(2)].each do |e|
  p fsf1[e]
  p fsf2[e]
end
