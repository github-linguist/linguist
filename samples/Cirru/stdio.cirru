
set a $ string 1
print a

print (string 1)

print nothing

print
  map
    a (int 4)
    b $ map
      a $ int 5
      b $ int 6
      c $ map
        int 7

print
  array
    int 1
    int 2
    array
      int 3
      int 4

print
  array
    int 1
    map
      a $ int 2
      b $ array
        int 3

print
  int 1
  int 2

print $ code
  set a 1
  print (get a)
  print $ array
    int a
    array
      int a

set container (map)
set container code $ code
  set a 1
  print (get a)
  print $ array
    int a
    array
      int a

print container