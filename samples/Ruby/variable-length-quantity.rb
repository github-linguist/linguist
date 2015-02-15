[2097152, 2097151].each do |i|
  # Encode i => BER
  ber = [i].pack("w")
  hex = ber.unpack("C*").collect {|c| "%02x" % c}.join(":")
  printf "%s => %s\n", i, hex

  # Decode BER => j
  j = ber.unpack("w").first
  i == j or fail "BER not preserve integer"
end
