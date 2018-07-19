import java.io.File

puts File.new('file-size.mirah').length()
puts File.new("./#{File.separator}file-size.mirah").length()
