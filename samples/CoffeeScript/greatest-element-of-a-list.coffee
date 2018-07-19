# using Math library
max1 = (list) ->
 Math.max.apply null, list

# using no libraries
max2 = (list) ->
 maxVal = list[0]
 for value in list
  maxVal = value if value > maxVal
 maxVal



# Test it
a = [0,1,2,5,4];
alert(max1(a)+". The answer is "+max2(a));
