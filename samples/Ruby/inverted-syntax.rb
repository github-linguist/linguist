# Raise ArgumentError if n is negative.
if n < 0 then raise ArgumentError, "negative n" end
raise ArgumentError, "negative n" if n < 0

# Exit 1 unless we can call Process.fork.
unless Process.respond_to? :fork then exit 1 end
exit 1 unless Process.respond_to? :fork

# Empty an array, printing each element.
while ary.length > 0 do puts ary.shift end
puts ary.shift while ary.length > 0

# Another way to empty an array, printing each element.
until ary.empty? do puts ary.shift end
puts ary.shift until ary.empty?
