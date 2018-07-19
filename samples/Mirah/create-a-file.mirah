import java.io.File

File.new('output.txt').createNewFile()
File.new('docs').mkdir()
File.new("docs#{File.separator}output.txt").createNewFile()
