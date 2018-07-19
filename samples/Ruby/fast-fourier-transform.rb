def fft(vec)
  return vec if vec.size <= 1
  evens_odds = vec.partition.with_index{|_,i| i.even?}
  evens, odds = evens_odds.map{|even_odd| fft(even_odd)*2}
  evens.zip(odds).map.with_index do |(even, odd),i|
    even + odd * Math::E ** Complex(0, 2 * Math::PI * (-i)/ vec.size)
  end
end

fft([1,1,1,1,0,0,0,0]).each{|c| p c}
