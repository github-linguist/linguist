url = "http://www.puzzlers.org/pub/wordlists/unixdict.txt"

wordlist = map!(chomp,(open(readlines, download(url)))) ;

function anagram(wordlist)
  hash = Dict() ; ananum = 0
  for word in wordlist
    sorted = CharString(sort(collect(word.data)))
    hash[sorted] = [ get(hash, sorted, {}), word ]
    ananum = max(length(hash[sorted]), ananum)
  end
  collect(values(filter((x,y)-> length(y) == ananum, hash)))
end
