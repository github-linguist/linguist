# We assume that a Fixnum occupies one machine word.
# Fixnum#size returns bytes (1 byte = 8 bits).
word_size = 42.size * 8
puts "Word size: #{word_size} bits"

# Array#pack knows the native byte order. We pack 1 as a 16-bit integer,
# then unpack bytes: [0, 1] is big endian, [1, 0] is little endian.
bytes = [1].pack('S').unpack('C*')
byte_order = (bytes[0] == 0 ? 'big' : 'little') + ' endian'
puts "Byte order: #{byte_order}"
