fullname = favouritefruit = ""
needspeeling = seedsremoved = false
otherfamily = []

IO.foreach("config.file") do |line|
  line.chomp!
  key, value = line.split(nil, 2)
  case key
  when /^([#;]|$)/; # ignore line
  when "FULLNAME"; fullname = value
  when "FAVOURITEFRUIT"; favouritefruit = value
  when "NEEDSPEELING"; needspeeling = true
  when "SEEDSREMOVED"; seedsremoved = true
  when "OTHERFAMILY"; otherfamily = value.split(",").map(&:strip)
  when /^./; puts "#{key}: unknown key"
  end
end

puts "fullname       = #{fullname}"
puts "favouritefruit = #{favouritefruit}"
puts "needspeeling   = #{needspeeling}"
puts "seedsremoved   = #{seedsremoved}"
otherfamily.each_with_index do |name, i|
  puts "otherfamily(#{i+1}) = #{name}"
end
