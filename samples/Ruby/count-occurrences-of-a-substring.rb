def countSubstrings str, subStr
    return str.scan(subStr).length
end

irb(main):001:0> "the three truths".scan("th").length
=> 3
irb(main):002:0> "ababababab".scan("abab").length
=> 2
