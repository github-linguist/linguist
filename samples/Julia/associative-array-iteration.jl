mydict = [ "hello"=>13, "world"=>31, "!"=>71 ]

# applying a function to key-value pairs:
 map(println, mydict);

# iterating over key-value pairs:
for (key,value) in mydict
  println("key = $key, value = $value")
end

# iterating over keys:
for key in keys(mydict)
  println("key = $key")
end

# iterating over values:
for value in values(mydict)
  println("value = $value")
end
