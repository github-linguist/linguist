def euler(y, a, b, h)
  a.step(b,h) do |t|
    puts "%7.3f %7.3f" % [t,y]
    y += h * yield(t,y)
  end
end

[10, 5, 2].each do |step|
  puts "Step = #{step}"
  euler(100,0,100,step) {|time, temp| -0.07 * (temp - 20) }
  puts
end
