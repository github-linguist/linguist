class Integer
  # Converts a normal integer to a Gray code.
  def to_gray
    raise Math::DomainError, "integer is negative" if self < 0
    self ^ (self >> 1)
  end

  # Converts a Gray code to a normal integer.
  def from_gray
    raise Math::DomainError, "integer is negative" if self < 0
    recurse = proc do |i|
      next 0 if i == 0
      o = recurse[i >> 1] << 1
      o | (i[0] ^ o[1])
    end
    recurse[self]
  end
end


(0..31).each do |number|
  encoded = number.to_gray
  decoded = encoded.from_gray
  printf("%2d: %5b => %5b => %5b: %2d\n",
         number, number, encoded, decoded, decoded)
end
