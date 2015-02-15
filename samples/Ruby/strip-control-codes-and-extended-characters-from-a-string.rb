class String

  def strip_control_characters()
    self.chars.inject("") do |str, char|
      unless char.ascii_only? and (char.ord < 32 or char.ord == 127)
        str << char
      end
      str
    end
  end

  def strip_control_and_extended_characters()
    self.chars.inject("") do |str, char|
      if char.ascii_only? and char.ord.between?(32,126)
        str << char
      end
      str
    end
  end
end

p s = "\ba\x00b\n\rc\fd\xc3\x7ffoo"
p s.strip_control_characters
p s.strip_control_and_extended_characters
