require 'priority_queue'

def huffman_encoding(str)
  char_count = Hash.new(0)
  str.each_char {|c| char_count[c] += 1}

  pq = CPriorityQueue.new
  # chars with fewest count have highest priority
  char_count.each {|char, count| pq.push(char, count)}

  while pq.length > 1
    key1, prio1 = pq.delete_min
    key2, prio2 = pq.delete_min
    pq.push([key1, key2], prio1 + prio2)
  end

  Hash[*generate_encoding(pq.min_key)]
end

def generate_encoding(ary, prefix="")
  case ary
  when Array
    generate_encoding(ary[0], "#{prefix}0") + generate_encoding(ary[1], "#{prefix}1")
  else
    [ary, prefix]
  end
end

def encode(str, encoding)
  str.each_char.collect {|char| encoding[char]}.join
end

def decode(encoded, encoding)
  rev_enc = encoding.invert
  decoded = ""
  pos = 0
  while pos < encoded.length
    key = ""
    while rev_enc[key].nil?
      key << encoded[pos]
      pos += 1
    end
    decoded << rev_enc[key]
  end
  decoded
end

str = "this is an example for huffman encoding"
encoding = huffman_encoding(str)
encoding.to_a.sort.each {|x| p x}

enc = encode(str, encoding)
dec = decode(enc, encoding)
puts "success!" if str == dec
