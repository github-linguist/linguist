s = "hello"
p s + " literal"        #=> "hello literal"
s1 = s + " literal"
p s1                    #=> "hello literal"
s1 << " another" # append to s1
p s1                    #=> "hello literal another"

s = "hello"
p s.concat(" literal")  #=> "hello literal"
p s                     #=> "hello literal"
