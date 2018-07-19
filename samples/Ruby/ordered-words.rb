require 'open-uri'
ordered_words = open('http://www.puzzlers.org/pub/wordlists/unixdict.txt', 'r').select do |word|
  word.chomp!
  word.split( '' ).sort.join == word
end

grouped = ordered_words.group_by{ |word| word.size }
puts grouped[grouped.keys.max]
