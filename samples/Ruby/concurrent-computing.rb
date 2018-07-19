%w{Enjoy Rosetta Code}.map do |x|
    Thread.new do
        sleep rand
        puts x
    end
end.each do |t|
  t.join
end
