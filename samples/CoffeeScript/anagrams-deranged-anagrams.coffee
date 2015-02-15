http = require 'http'

is_derangement = (word1, word2) ->
  for c, i in word1
    return false if c == word2[i]
  true

show_longest_derangement = (word_lst) ->
  anagrams = {}
  max_len = 0

  for word in word_lst
    continue if word.length < max_len
    key = word.split('').sort().join('')
    if anagrams[key]
      for prior in anagrams[key]
        if is_derangement(prior, word)
          max_len = word.length
          result = [prior, word]
    else
      anagrams[key] = []
    anagrams[key].push word

  console.log "Longest derangement: #{result.join ' '}"

get_word_list = (process) ->
  options =
    host: "www.puzzlers.org"
    path: "/pub/wordlists/unixdict.txt"

  req = http.request options, (res) ->
    s = ''
    res.on 'data', (chunk) ->
      s += chunk
    res.on 'end', ->
      process s.split '\n'
  req.end()

get_word_list show_longest_derangement
