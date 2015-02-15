def getNthLine(filename, n)
  line=""
  File.open(filename) do |f|
    n.times do |nr|
      line = f.gets
      if line.nil?
        puts "file #{filename} does not have #{n} lines, only #{nr}"
        break
      end
    end
  end
  line
end

puts getNthLine("/etc/passwd", 7)
