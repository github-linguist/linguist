|s1 s2|
"bind the var s1 to the object string on the right"
s1 := 'i am a string'.
"bind the var s2 to the same object..."
s2 := s1.
"bind s2 to a copy of the object bound to s1"
s2 := (s1 copy).
