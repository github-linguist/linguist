class String

  SoundexChars = 'BFPVCGJKQSXZDTLMNR'
  SoundexNums  = '111122222222334556'
  SoundexCharsEx = '^' + SoundexChars
  SoundexCharsDel = '^A-Z'

  # desc: http://en.wikipedia.org/wiki/Soundex
  def soundex(census = true)
    str = self.upcase.delete(SoundexCharsDel)
    str[0,1] + str[1..-1].delete(SoundexCharsEx).
                          tr_s(SoundexChars, SoundexNums)\
                          [0 .. (census ? 2 : -1)].
                          ljust(3, '0') rescue ''
  end

  def sounds_like(other)
    self.soundex == other.soundex
  end
end

%w(Soundex Sownteks Example Ekzampul foo bar).each_slice(2) do |word1, word2|
  [word1, word2].each {|word| puts '%-8s -> %s' % [word, word.soundex]}

  print "'#{word1}' "
  print word1.sounds_like(word2) ? "sounds" : "does not sound"
  print " like '#{word2}'\n"
end
