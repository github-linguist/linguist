def crunch(ascii)
  bitstring = ascii.bytes.collect {|b| "%07b" % b}.join
  [bitstring].pack("B*")
end

def expand(binary)
  bitstring = binary.unpack("B*")[0]
  bitstring.scan(/[01]{7}/).collect {|b| b.to_i(2).chr}.join
end

original = "This is an ascii string that will be crunched, written, read and expanded."
puts "my ascii string is #{original.length} bytes"

filename = "crunched.out"

# write the compressed data
File.open(filename, "w") do |fh|
  fh.binmode
  fh.print crunch(original)
end

filesize = File.size(filename)
puts "the file containing the crunched text is #{filesize} bytes"

# read and expand
expanded = File.open(filename, "r") do |fh|
  fh.binmode
  expand(fh.read)
end

if original == expanded
  puts "success"
else
  puts "fail!"
end
