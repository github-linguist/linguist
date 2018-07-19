require 'tempfile'

def remove_lines(filename, start, num)
  tmp = Tempfile.open(filename) do |fp|
    File.foreach(filename) do |line|
      if $. >= start and num > 0
        num -= 1
      else
        fp.puts line
      end
    end
    fp
  end
  puts "Warning: End of file encountered before all lines removed" if num > 0
  FileUtils.copy(tmp.path, filename)
  tmp.unlink
end

# Test code
def setup(filename, start, remove)
  puts "remove #{remove} lines starting at line #{start}"
  File.open(filename, "w") {|fh| (1..5).each {|i| fh.puts " "*i + i.to_s}}
  puts "before:", File.read(filename)
end

def teardown(filename)
  puts "after:", File.read(filename)
  puts
  File.unlink(filename)
end

filename = "foobar.txt"
start = 2
[2, 6].each do |remove|
  setup(filename, start, remove)
  remove_lines(filename, start, remove)
  teardown(filename)
end
