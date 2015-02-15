def pangram?(sentence)
  unused_letters = ('a'..'z').to_a - sentence.downcase.chars.to_a
  unused_letters.empty?
end

p pangram?('this is a sentence')  # ==> false
p pangram?('The quick brown fox jumps over the lazy dog.')  # ==> true
