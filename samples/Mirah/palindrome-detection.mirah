def reverse(s:string)
    StringBuilder.new(s).reverse.toString()
end

def palindrome?(s:string)
    s.equals(reverse(s))
end

puts palindrome?("anna")        # ==> true
puts palindrome?("Erik")        # ==> false
puts palindrome?("palindroom-moordnilap") # ==> true
puts nil                        # ==> null
