require 'open-uri'
anagram = open('http://www.puzzlers.org/pub/wordlists/unixdict.txt') do |f|
  f.read.split.group_by {|s| s.each_char.sort}
end

def deranged?(a, b)
  a.chars.zip(b.chars).all? {|char_a, char_b| char_a != char_b}
end

def find_derangements(list)
  for i in 0 ... list.size-1
    for j in i ... list.size
      return list[i], list[j]  if deranged?(list[i], list[j])
    end
  end
  nil
end

anagram = anagram.select{|k,list| list.size>1}.sort_by{|k,list| -k.size}

anagram.each do |k,list|
  derangements = find_derangements(list)
  if derangements
    puts "derangement with longest word size: #{derangements}"
    break
  end
end
