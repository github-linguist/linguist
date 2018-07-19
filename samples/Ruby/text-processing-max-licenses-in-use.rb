out = 0
max_out = -1
max_times = []

File.foreach('mlijobs.txt') do |line|
  out += line.include?("OUT") ? 1 : -1
  if out > max_out
    max_out = out
    max_times = []
  end
  max_times << line.split[3]  if out == max_out
end

puts "Maximum simultaneous license use is #{max_out} at the following times:"
max_times.each {|time| puts "  #{time}"}
